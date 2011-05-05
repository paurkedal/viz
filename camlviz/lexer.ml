(* Copyright 2010--2011  Petter Urkedal
 *
 * This file is part of the Viz Compiler <http://www.vizlang.org/>.
 *
 * The Viz Compiler is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * The Viz Compiler is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with the Viz Compiler.  If not, see <http://www.gnu.org/licenses/>.
 *)

open FfPervasives
open Unicode
module UCharInfo = CamomileLibrary.Default.Camomile.UCharInfo
open Diag
open Cst_types
open Cst_core
open Leaf_types
open Leaf_core

let dlog_en = Diag.dlog_en_for "Camlviz.Lexer"
let dlogf fmt ?loc = Diag.dlogf_for "Camlviz.Lexer" ?loc fmt

type tokeninfo =
    | Ti_operator of Grammar.token * Opkind.t
    | Ti_keyword of Grammar.token * Opkind.lexical_role

let tokeninfo_token = function
    | Ti_operator (tok, _) -> tok
    | Ti_keyword (tok, _) -> tok

type pending_work =
    | Pending_BEGIN
	(* Pushed on to of a Pending_END to protect it from taking effect on
	 * the next intro-word.  That is, the next block will be wrapped
	 * inside the current block.  This is used for "what", "which",
	 * "where", and "with". *)
    | Pending_END of Location.t * int
	(* Emit an end-of-block when reaching an intro-word before the given
	 * column. *)

type state = {
    st_stream : LStream.t;

    (* The current indentation. *)
    mutable st_indent : int;

    (* A look-ahead token. *)
    mutable st_holding : Location.t * Grammar.token * Opkind.lexical_role;

    (* Things to take into account before reading any further. *)
    mutable st_pending : pending_work list;

    (* The lexical role of the last seen introducer. *)
    mutable st_last_lexical_role : Opkind.lexical_role;

    (* The currently active keywords and operators. *)
    mutable st_keywords : tokeninfo UString_trie.t;

    (* Operator renames, primarily meant for supporting ASCII variants. *)
    mutable st_renames : idr Idr_map.t;

    mutable st_lookahead : int;
    mutable st_scanners : (state -> Grammar.token option) list;

    (* This is the location of the last emitted token.  It's used when the
     * grammar raises Grammar.Error, otherwise we pass along the location and
     * use Error_at. *)
    mutable st_last_location : Location.t;
}

let last_location state = state.st_last_location

let initial_inerts = [
    "true",	Grammar.LITERAL (Lit_bool true);
    "false",	Grammar.LITERAL (Lit_bool false);
]
let initial_declarators = [
    "open",	Grammar.OPEN Abi_Viz;
    "open:c",	Grammar.OPEN Abi_C;
    "sealed",	Grammar.SEALED;
    "include",	Grammar.INCLUDE;
    "use",	Grammar.USE;
    "in",	Grammar.IN;
    "sig",	Grammar.SIG;
    "type",	Grammar.TYPE Abi_Viz;
    "type:c",	Grammar.TYPE Abi_C;
    "let",	Grammar.LET None;
    "let!",	Grammar.LET (Some "");
    "val",	Grammar.VAL (`Default,  Abi_Viz, []);
    "val-",	Grammar.VAL (`Local,    Abi_Viz, []);
    "val:c",	Grammar.VAL (`Default,  Abi_C, []);
    "val:cf",	Grammar.VAL (`Default,  Abi_C, [`Is_finalizer]);
    "val:cs",	Grammar.VAL (`Default,  Abi_C, [`Is_stub]);
    "val:c-",	Grammar.VAL (`Local,    Abi_C, []);
    "val:cf-",	Grammar.VAL (`Local,    Abi_C, [`Is_finalizer]);
    "inj",	Grammar.INJ Abi_Viz;
    "inj:c",	Grammar.INJ Abi_C;
    "when",	Grammar.WHEN "";
    "upon",	Grammar.UPON;
    "lex",	Grammar.LEX;
    "lex alias",Grammar.LEXALIAS;
    "lex open",	Grammar.LEXOPEN;
    "otherwise",Grammar.OTHERWISE;
    "#?ffoc open", Grammar.OPEN Abi_Viz;
    "#?ffoc include", Grammar.INCLUDE;
    "#?ffoc type", Grammar.TYPE Abi_Viz;
    "#?ffoc let", Grammar.LET None;
    "#?ffoc let!", Grammar.LET (Some "");
    "#?ffoc {#", Grammar.SKIP;
    "#?ffoc #}", Grammar.ENDSKIP;
]
let initial_verbs = [
    "assert",	Grammar.ASSERT;
    "be",	Grammar.BE;
    "do",	Grammar.DO "";
    "fail",	Grammar.FAIL;
    "raise",	Grammar.RAISE;
    "__trace",	Grammar.TRACE;
]
let initial_connectives = [
    ".at",	Grammar.DOT_AT;
    "where",	Grammar.WHERE;
    "with",	Grammar.WITH;
    "what",	Grammar.WHAT None;
    "what!",	Grammar.WHAT (Some "");
    "which",	Grammar.WHICH None;
    "which!",	Grammar.WHICH (Some "");
]
let initial_conditionals = [
    "at",	Grammar.AT;
    "if",	Grammar.IF;
    "else",	Grammar.ELSE;
]
let initial_lookahead = 40

let initial_keywords =
    let addkw lr (name, token) =
	UString_trie.add_utf8 name (Ti_keyword (token, lr)) in
    List.fold (addkw Opkind.Lr_inert)		initial_inerts @<
    List.fold (addkw Opkind.Lr_declarator)	initial_declarators @<
    List.fold (addkw Opkind.Lr_verb)		initial_verbs @<
    List.fold (addkw Opkind.Lr_connective)	initial_connectives @<
    List.fold (addkw Opkind.Lr_conditional)	initial_conditionals @<
    UString_trie.empty

let rec skip_line state =
    match LStream.pop state.st_stream with
    | None -> ()
    | Some ch -> if int_of_uchar ch != 0xa then skip_line state

let skip_space state =
    (* Skip spaces and comments, update indentation. *)
    let start_locb = LStream.locbound state.st_stream in
    let rec loop () =
	LStream.skip_while UChar.is_space state.st_stream;
	let com_loclb = LStream.locbound state.st_stream in
	match LStream.peek_n 3 state.st_stream with
	| ch0 :: ch1 :: rest (* Skip nested "{#"-"#}"-comment. *)
		when ch0 = UChar.ch_lbrace && ch1 = UChar.ch_hash
		  && (rest = [] || List.hd rest <> UChar.ch_rbrace) ->
	    LStream.skip_n 2 state.st_stream;
	    let com_locub = LStream.locbound state.st_stream in
	    let rec state0 n = match LStream.pop state.st_stream with
		| None -> stateEOF n
		| Some ch when ch = UChar.ch_hash -> stateH n
		| Some ch when ch = UChar.ch_lbrace -> stateL n
		| _ -> state0 n
	    and stateH n = match LStream.pop state.st_stream with
		| None -> stateEOF n
		| Some ch when ch = UChar.ch_hash -> stateH n
		| Some ch when ch = UChar.ch_rbrace -> stateHR n
		| _ -> state0 n
	    and stateL n = match LStream.pop state.st_stream with
		| None -> stateEOF n
		| Some ch when ch = UChar.ch_hash -> stateLH (n + 1)
		| Some ch when ch = UChar.ch_lbrace -> stateL n
		| _ -> state0 n
	    and stateLH n = match LStream.pop state.st_stream with
		| None -> stateEOF n
		| Some ch when ch = UChar.ch_rbrace ->
		    let loc = Location.at (LStream.locbound state.st_stream) in
		    raise (Error_at (loc, "Invalid {#} sequence in comment."))
		| Some ch when ch = UChar.ch_hash -> stateH n
		| Some ch when ch = UChar.ch_lbrace -> stateL n
		| _ -> state0 n
	    and stateHR n = if n > 0 then state0 (n - 1) else ()
	    and stateEOF n =
		let loc = Location.between com_loclb com_locub in
		raise (Error_at (loc, "Unterminated comment.")) in
	    state0 0;
	    loop ()
	| ch0 :: ch1 :: rest (* Skip "#"-comment. *)
		when ch0 = UChar.ch_hash && UChar.is_space ch1 ->
	    skip_line state;
	    loop ()
	| ch0 :: ch1 :: rest (* Skip "##" or "--"-comment. *)
		when (ch0 = UChar.ch_dash && ch1 = UChar.ch_dash ||
		      ch0 = UChar.ch_hash && ch1 = UChar.ch_hash)
		  && (rest = [] || List.hd rest <> UChar.ch_qmark) ->
	    skip_line state;
	    loop ()
	| _ -> () in
    loop ();
    let end_locb = LStream.locbound state.st_stream in
    if Location.Bound.lineno start_locb < Location.Bound.lineno end_locb then
	state.st_indent <- Location.Bound.column end_locb

let holding_column state =
    match state.st_holding with
    | (_, Grammar.EOF, _) -> -1
    | (loc, _, _) -> Location.Bound.column (Location.lbound loc)

let declare_operator state ok (Cidr (loc, op), names) =
    let op' = UString_sequence.create (idr_to_ustring op) in
    if dlog_en then
	dlogf "Declaring operator %s at %s." (idr_to_string op)
	      (Opkind.to_string ok);
    let name_idrs = List.map (fun (Cidr (_, name)) -> name) names in
    let ti =
	try Ti_operator (ok.Opkind.ok_create (op, name_idrs), ok)
	with Opkind.Invalid_definition msg -> raise (Error_at (loc, msg)) in
    state.st_keywords <- UString_trie.add op' ti state.st_keywords

let alias_operator state (Cidr (_, op), Cidr (loc_orig, op_orig)) =
    let op_orig' = UString_sequence.create (idr_to_ustring op_orig) in
    let op' = UString_sequence.create (idr_to_ustring op) in
    let ti =
	match UString_trie.find op_orig' state.st_keywords with
	| Some (Ti_operator (tok, ok)) ->
	    Ti_operator (tok, ok)
	| Some (Ti_keyword _) ->
	    raise (Error_at (loc_orig, "Cannot alias keyword."))
	| None ->
	    raise (Error_at (loc_orig, "Cannot alias undefined operator.")) in
    if dlog_en then
	dlogf "Aliasing operator %s as %s."
	      (idr_to_string op) (idr_to_string op_orig);
    state.st_keywords <- UString_trie.add op' ti state.st_keywords;
    state.st_renames <-
	List.fold (fun f -> Idr_map.add (f op) (f op_orig))
		  [idr_1o; idr_2o] state.st_renames

let skip_hspace state =
    LStream.skip_while UChar.is_hspace state.st_stream;
    begin match LStream.peek state.st_stream with
    | None -> false
    | Some ch -> ch != UChar.ch_nl
    end

let scan_space_separated_identifiers state =
    let rec scan_names accu =
	if not (skip_hspace state) then List.rev accu else
	let name, name_loc =
	    LStream.scan_while (not *< UChar.is_space) state.st_stream in
	let cidr = Cidr (name_loc, idr_of_ustring name) in
	scan_names (cidr :: accu) in
    scan_names []

let scan_lexops state =
    if not (skip_hspace state) then [] else
    let rec scan_names (names : cidr list) =
	if LStream.peek_code state.st_stream = 0x29 then names else
	if not (skip_hspace state) then
	    let loc = Location.at (LStream.locbound state.st_stream) in
	    raise (Error_at (loc, "Missing end parenthesis.")) else
	let f ch = not (UChar.is_space ch || UChar.code ch = 0x29) in
	let s, loc = LStream.scan_while f state.st_stream in
	let name = Cidr (loc, idr_of_ustring s) in
	if dlog_en then
	    dlogf ~loc:loc "Scanned operator name %s." (UString.to_utf8 s);
	scan_names (name :: names) in
    let rec scan_ops ops =
	let opname, opname_loc =
	    LStream.scan_while (not *< UChar.is_space) state.st_stream in
	let op = Cidr (opname_loc, idr_of_ustring opname) in
	if dlog_en then
	    dlogf ~loc:opname_loc "Scanned operator %s in lexdef."
		  (UString.to_utf8 opname);
	let names, cont =
	    if not (skip_hspace state) then ([], false) else
	    if LStream.peek_code state.st_stream <> 0x28 then ([], true) else
	    begin
		LStream.skip state.st_stream;
		let names = List.rev (scan_names []) in
		LStream.skip state.st_stream;
		(names, skip_hspace state)
	    end in
	let ops' = (op, names) :: ops in
	if cont then scan_ops ops' else ops' in
    List.rev (scan_ops [])

let scan_lexdef state lex_loc =
    skip_space state;
    let okname_u, okname_loc =
	LStream.scan_while (not *< UChar.is_space) state.st_stream in
    let loc_lb = LStream.locbound state.st_stream in
    let ops = scan_lexops state in
    let okname = UString.to_utf8 okname_u in
    let loc_ub = LStream.locbound state.st_stream in
    let loc = Location.between loc_lb loc_ub in
    let lexdef = Cdef_lex (loc, okname, ops) in
    let ok =
	try Opkind.of_string okname
	with Opkind.Domain_error ->
	    raise (Error_at (okname_loc, "Not an operator kind.")) in
    List.iter (declare_operator state ok) ops;
    (Grammar.PREPARED_DEF lexdef, loc)

let scan_lexalias state lex_loc =
    skip_space state;
    match List.even_odd_pairs (scan_space_separated_identifiers state) with
    | _, Some op ->
	errf_at (cidr_loc op) "lex alias takes a even number of operators."
    | op_pairs, None ->
	List.iter (alias_operator state) op_pairs;
	let loc = Location.between
		(Location.lbound lex_loc)
		(LStream.locbound state.st_stream) in
	let lexalias = Cdef_lexalias (loc, op_pairs) in
	(Grammar.PREPARED_DEF lexalias, loc)

let lexopen state = function
    | Ctrm_where (_, defs) -> List.iter
	begin function
	    | Cdef_lex (loc, okname, ops) ->
		let ok = Opkind.of_string okname in
		List.iter (declare_operator state ok) ops
	    | Cdef_lexalias (loc, op_pairs) ->
		List.iter (alias_operator state) op_pairs
	    | _ -> ()
	end
	defs
    | ctrm ->
	let loc = ctrm_loc ctrm in
	raise (Error_at (loc, "Lexical open needs a structure."))

let triescan state trie =
    let la = Sequence.of_list
			(LStream.peek_n state.st_lookahead state.st_stream) in
    let check_prefix la ki_opt (len, last_ch, res) =
	let this_ch = Option.default UChar.ch_space (Sequence.peek la) in
	if UChar.are_tied last_ch this_ch
	then (len + 1, this_ch, res)
	else
	    let res' =
		match ki_opt with
		| None -> res
		| Some kwi -> Some (len, kwi) in
	    (len + 1, this_ch, res') in
    match UString_trie.prefix_optfold check_prefix la trie
				      (0, UChar.ch_space, None) with
    | (_, _, None) -> None
    | (_, _, Some (len, res)) ->
	let loc_lb = LStream.locbound state.st_stream in
	LStream.skip_n len state.st_stream;
	let loc_ub = LStream.locbound state.st_stream in
	Some (res, UString.of_sequence_n len la, Location.between loc_lb loc_ub)

let scan_custom state =
    (* Special handling of ".<space>". *)
    let loc_lb = LStream.locbound state.st_stream in
    begin match LStream.peek_n 2 state.st_stream with
    | [ch0; ch1] when UChar.code ch0 = 0x2e && UChar.is_space ch1 ->
	LStream.skip state.st_stream;
	let loc_ub = LStream.locbound state.st_stream in
	Some (Location.between loc_lb loc_ub, Grammar.DOT, Opkind.Lr_inert)
    | _ -> None
    end

let scan_keyword state =
    let unwrap (ti, tokstr, loc) =
	match ti with
	| Ti_keyword (Grammar.LEX, lr) ->
	    let (tok, loc_spec) = scan_lexdef state loc in
	    let loc_full = Location.span [loc; loc_spec] in
	    (loc_full, tok, lr)
	| Ti_keyword (Grammar.LEXALIAS, lr) ->
	    let (tok, loc_spec) = scan_lexalias state loc in
	    let loc_full = Location.span [loc; loc_spec] in
	    (loc_full, tok, lr)
	| Ti_keyword (tok, lr) -> (loc, tok, lr)
	| Ti_operator (tok, ok) -> (loc, tok, ok.Opkind.ok_lexical_role) in
    Option.map unwrap (triescan state state.st_keywords)

let scan_regular_identifier state =
    let (name, loc) = LStream.scan_while UChar.is_idrchr state.st_stream in
    idr_of_ustring name

let scan_operator_identifier state =
    let buf = UString.Buf.create 8 in
    UString.Buf.add_char buf (LStream.pop_e state.st_stream);
    UString.Buf.add_char buf (LStream.pop_e state.st_stream);
    let level = ref 0 in
    LStream.skip_while
	begin fun ch ->
	    if match UCharInfo.general_category ch with
	    | `Ps | `Pi -> level := !level + 1; true
	    | `Pe | `Pf -> level := !level - 1; !level >= 0
	    | `Cc | `Zs | `Zl | `Zp -> false
	    | _ -> true
	    then (UString.Buf.add_char buf ch; true)
	    else false
	end
	state.st_stream;
    let idr = idr_of_ustring (UString.Buf.contents buf) in
    try Idr_map.find idr state.st_renames with Not_found -> idr

let scan_identifier state =
    match LStream.peek_n 3 state.st_stream with
    | [] -> Some Grammar.EOF
    | ch_digit :: ch_o :: _
	    when UChar.is_ascii_digit ch_digit && UChar.code ch_o = 0x27 ->
	Some (Grammar.IDENTIFIER (scan_operator_identifier state))
    | ch_dot :: ch_digit :: ch_o :: _
	    when UChar.code ch_dot = 0x2e
	      && UChar.is_ascii_digit ch_digit && UChar.code ch_o = 0x27 ->
	LStream.skip state.st_stream;
	Some (Grammar.PROJECT (scan_operator_identifier state))
    | ch_dot :: ch_idrchr :: _
	    when UChar.code ch_dot = 0x2e && UChar.is_idrchr ch_idrchr ->
	LStream.skip state.st_stream;
	Some (Grammar.PROJECT (scan_regular_identifier state))
    | ch_apo :: ch_idrchr :: _
	    when UChar.code ch_apo = 0x27 && UChar.is_idrchr ch_idrchr ->
	LStream.skip state.st_stream;
	let idr = scan_regular_identifier state in
	Some (Grammar.HINTED_IDENTIFIER (idr, Ih_univ))
    | ch0 :: _ when UChar.is_idrchr ch0 ->
	let idr = scan_regular_identifier state in
	if LStream.peek_code state.st_stream = 0x25 then begin
	    LStream.skip state.st_stream;
	    Some (Grammar.HINTED_IDENTIFIER (idr, Ih_inj))
	end else if UChar.is_greek_alpha ch0 then
	    Some (Grammar.HINTED_IDENTIFIER (idr, Ih_univ))
	else
	    Some (Grammar.IDENTIFIER idr)
    | _ -> None

let escape_map =
    List.fold (fun (x, y) -> UChar_map.add (UChar.of_char x) (UChar.of_char y)) [
	'"', '"'; '\\', '\\';
	'n', '\n'; 'r', '\r'; 't', '\t';
    ] UChar_map.empty

let scan_string_literal state =
    let buf = UString.Buf.create 8 in
    let loc_lb = LStream.locbound state.st_stream in
    LStream.skip state.st_stream;
    let rec next () =
	match LStream.pop state.st_stream with
	| None ->
	    let loc = Location.between loc_lb loc_lb in
	    raise (Error_at (loc, "Unterminated string literal."))
	| Some ch ->
	    if ch = UChar.of_char '\\' then
		begin match LStream.pop state.st_stream with
		| None ->
		    let loc = Location.between loc_lb loc_lb in
		    raise (Error_at (loc, "Unterminated string escape."))
		| Some ch' ->
		    try
			let ch'' = UChar_map.find ch' escape_map in
			UString.Buf.add_char buf ch'';
			next ()
		    with Not_found ->
			let loc = Location.between loc_lb loc_lb in
			raise (Error_at (loc, "Unterminated string escape."))
		end
	    else if ch <> UChar.of_char '"' then begin
		UString.Buf.add_char buf ch;
		next ()
	    end in
    next ();
    Lit_string (UString.Buf.contents buf)

let scan_char_literal state =
    assert (LStream.peek_code state.st_stream = 0x63);
    let loc_lb = LStream.locbound state.st_stream in
    LStream.skip state.st_stream;
    match scan_string_literal state with
    | Lit_string s ->
	if UString.length s <> 1 then
	    let loc_ub = LStream.locbound state.st_stream in
	    raise (Error_at (Location.between loc_lb loc_ub,
			     "Expecting a single character."))
	else
	    Lit_char (UString.get s 0)
    | _ -> assert false

let scan_int have_sign base state =
    let rec f accu =
	let ch = Option.default UChar.ch_space (LStream.peek state.st_stream) in
	if not (UChar.is_idrchr ch) then accu else
	let code = UChar.code ch in
	let value =
	    if 0x30 <= code && code <= 0x39 then code - 0x30 else
	    if 0x61 <= code && code <= 0x66 then code - 0x61 + 10 else
	    raise Grammar.Error in
	if value >= base then raise Grammar.Error else
	(LStream.skip state.st_stream; f (value + base * accu)) in
    let n = (f 0) in
    Lit_int (if have_sign then -n else n)

let scan_literal state =
    let found lit =
	Some (Grammar.LITERAL lit) in
    let found_int n_skip have_sign base =
	LStream.skip_n n_skip state.st_stream;
	found (scan_int have_sign base state) in
    match LStream.peek_n_code 3 state.st_stream with
    | 0x22 :: _ -> found (scan_string_literal state)
    | 0x63 :: 0x22 :: _ -> found (scan_char_literal state)
    | 0x30 :: 0x62 :: _ -> found_int 2 false 2
    | 0x30 :: 0x6f :: _ -> found_int 2 false 8
    | 0x30 :: 0x78 :: _ -> found_int 2 false 16
    | ch0 :: ch1 :: _ when 0x30 <= ch0 && ch0 <= 0x39 && ch1 != 0x27 ->
	found_int 0 false 10
    | 0x2d :: 0x30 :: 0x62 :: _ -> found_int 3 true 2
    | 0x2d :: 0x30 :: 0x6f :: _ -> found_int 3 true 8
    | 0x2d :: 0x30 :: 0x78 :: _ -> found_int 3 true 16
    | 0x2d :: ch0 :: ch1 :: _ when 0x30 <= ch0 && ch0 <= 0x39 && ch1 != 0x27 ->
	found_int 1 true 10
    | _ -> None

let scan_eof state =
    if LStream.peek state.st_stream = None then
	let loc = Location.at (LStream.locbound state.st_stream) in
	Some (loc, Grammar.EOF, Opkind.Lr_declarator)
    else
	None

let fixed_scanners = [scan_eof; scan_custom; scan_keyword]
let default_scanners = [scan_literal; scan_identifier]

let pop_manifest_token state =
    skip_space state;
    let (loc', tok', lr') = state.st_holding in
    begin match List.find_image (fun f -> f state) fixed_scanners with
    | Some (loc, tok, lr) ->
	state.st_holding <- (loc, tok, lr)
    | None ->
	let loc_lb = LStream.locbound state.st_stream in
	let tok =
	    try
		match List.find_image (fun f -> f state) state.st_scanners with
		| Some x -> x
		| None ->
		    raise Grammar.Error
	    with Grammar.Error ->
		let loc_ub = LStream.locbound state.st_stream in
		let loc = Location.between loc_lb loc_ub in
		raise (Error_at (loc, "Invalid literal."))
	    in
	let loc_ub = LStream.locbound state.st_stream in
	let loc = Location.between loc_lb loc_ub in
	if dlog_en then dlogf ~loc "Scanned regular token.";
	state.st_holding <- (loc, tok, Opkind.Lr_inert)
    end;
    begin match lr' with
    | Opkind.Lr_inert -> ()
    | _ -> state.st_last_lexical_role <- lr'
    end;
    (loc', tok')

let pop_virtual_token state =
    let cur_col = holding_column state in
    let (loc, tok, lr) = state.st_holding in
    let loc_lb = Location.lbound loc in
    let follows_verb = state.st_last_lexical_role = Opkind.Lr_verb in
    if not (Opkind.is_introducer lr) then begin
	if not (Opkind.is_connective ~follows_verb lr) then
	    pop_manifest_token state else
	match state.st_pending with
	| Pending_END (loc_begin, col) :: pending when cur_col < col ->
	    state.st_pending <- Pending_BEGIN :: pending;
	    if dlog_en then dlogf ~loc "END[col = %d]" col;
	    (loc, Grammar.END)
	| _ ->
	    state.st_pending <- Pending_BEGIN :: state.st_pending;
	    pop_manifest_token state
    end else
	match state.st_pending with
	| Pending_BEGIN :: pending ->
	    let loc_begin = Location.between loc_lb loc_lb in
	    state.st_pending <- Pending_END (loc_begin, cur_col)
			     :: pending;
	    if dlog_en then dlogf ~loc:loc_begin "BEGIN[col = %d]" cur_col;
	    (loc_begin, Grammar.BEGIN)
	| Pending_END (_, col) :: pending when cur_col = col ->
	    if dlog_en then dlogf ~loc "ITEM[col = %d]" col;
	    if Opkind.is_connective ~follows_verb lr then
		state.st_pending <- Pending_BEGIN :: state.st_pending;
	    pop_manifest_token state
	| Pending_END (loc_begin, col) :: pending when cur_col < col ->
	    state.st_pending <- pending;
	    if dlog_en then dlogf ~loc "END[col = %d]" col;
	    (loc, Grammar.END)
	| Pending_END _ :: _ | [] as pending ->
	    if tok = Grammar.EOF then (loc, tok) else
	    let loc_begin = Location.between loc_lb loc_lb in
	    state.st_pending <- Pending_END (loc_begin, cur_col)
			     :: pending;
	    if dlog_en then dlogf ~loc:loc_begin "BEGIN[col = %d]" cur_col;
	    (loc_begin, Grammar.BEGIN)

let default_state_template = {
    st_stream = LStream.null;
    st_indent = -1;
    st_holding = (Location.dummy, Grammar.EOF, Opkind.Lr_declarator);
    st_pending = [];
    st_last_lexical_role = Opkind.Lr_inert;
    st_keywords = initial_keywords;
    st_renames = Idr_map.empty;
    st_lookahead = initial_lookahead;
    st_scanners = default_scanners;
    st_last_location = Location.dummy;
}

let create_from_lstream st_stream =
    let state = {default_state_template with st_stream = st_stream} in
    let _ = pop_manifest_token state in state

let create_from_file path =
    create_from_lstream (LStream.open_in path)

let lexer state () =
    let (loc, tok) = pop_virtual_token state in
    state.st_last_location <- loc;
    (tok, loc)
