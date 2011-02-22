(* Copyright 2010--2011  Petter Urkedal
 *
 * This file is part of Fform/OC <http://www.eideticdew.org/p/fform/>.
 *
 * Fform/OC is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fform/OC is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Fform/OC.  If not, see <http://www.gnu.org/licenses/>.
 *)

open FfPervasives
open Unicode
module UCharInfo = CamomileLibrary.Default.Camomile.UCharInfo
open Diag
open Cst_types
open Cst_core
open Leaf_types
open Leaf_core

let dlog_en = Diag.dlog_en_for "Fform.Lexer"
let dlogf fmt ?loc = Diag.dlogf_for "Fform.Lexer" ?loc fmt

type tokeninfo =
    | Ti_operator of Grammar.token * Opkind.t
    | Ti_keyword of Grammar.token * Opkind.lexkind

let tokeninfo_token = function
    | Ti_operator (tok, _) -> tok
    | Ti_keyword (tok, _) -> tok
let tokeninfo_lexkind = function
    | Ti_operator (_, ok) -> ok.Opkind.ok_lexkind
    | Ti_keyword (_, lk) -> lk

type state = {
    st_stream : LStream.t;

    (* The current indentation, and stack of past indentations. *)
    mutable st_indent : int;
    mutable st_indent_stack : (int * Location.t) list;

    (* Token to be emitted before reading any further. *)
    mutable st_stash : (Grammar.token * Location.t) option;

    (* True after a "what", "which", "where", or "with".  This prevents popping
     * the indent_stack, so that one can use unindentation. *)
    mutable st_continued : bool;

    (* The currently active keywords and operators. *)
    mutable st_keywords : tokeninfo UString_trie.t;

    mutable st_lookahead : int;
    mutable st_last_location : Location.t;
    mutable st_scanners : (state -> Grammar.token option) list;
}

let last_location state = state.st_last_location

let initial_intro_keywords = [
    "open",	Grammar.OPEN;
    "include",	Grammar.INCLUDE;
    "in",	Grammar.IN;
    "sig",	Grammar.SIG;
    "type",	Grammar.TYPE;
    "let",	Grammar.LET None;
    "let!",	Grammar.LET (Some "");
    "val",	Grammar.VAL None;
    "val!",	Grammar.VAL (Some "");
    "inj",	Grammar.INJ;
    "be",	Grammar.BE;
    "do",	Grammar.DO "";
    "raise",	Grammar.RAISE;
    "upon",	Grammar.UPON;
    "lex",	Grammar.LEX;
    "lex alias",Grammar.LEXALIAS;
    "lex open",	Grammar.LEXOPEN;
    "if",	Grammar.IF;
    "else",	Grammar.ELSE;
    "otherwise",Grammar.OTHERWISE;
    "at",	Grammar.AT;
    "--?FFOC open", Grammar.OPEN;
    "--?FFOC include", Grammar.INCLUDE;
    "--?FFOC type", Grammar.TYPE;
    "--?FFOC {#", Grammar.SKIP;
    "--?FFOC #}", Grammar.ENDSKIP;
]
let initial_plain_keywords = [
    "true",	Grammar.LITERAL (Lit_bool true);
    "false",	Grammar.LITERAL (Lit_bool false);
]
let initial_continued_keywords = [
    "where",	Grammar.WHERE;
    "with",	Grammar.WITH;
    "what",	Grammar.WHAT None;
    "what!",	Grammar.WHAT (Some "");
    "which",	Grammar.WHICH None;
    "which!",	Grammar.WHICH (Some "");
]
let initial_lookahead = 40

let initial_keywords =
    let addkw lk (name, token) =
	UString_trie.add_utf8 name (Ti_keyword (token, lk)) in
    List.fold (addkw Opkind.Lex_intro) initial_intro_keywords @<
    List.fold (addkw Opkind.Lex_regular) initial_plain_keywords @<
    List.fold (addkw Opkind.Lex_continued) initial_continued_keywords @<
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
	match LStream.peek_n 3 state.st_stream with
	| ch0 :: ch1 :: rest
		when ch0 = UChar.ch_dash && ch1 = UChar.ch_dash
		  && (rest = [] || UChar.code (List.hd rest) <> 0x3f) ->
	    skip_line state;
	    loop ()
	| _ -> () in
    loop ();
    let end_locb = LStream.locbound state.st_stream in
    if Location.Bound.lineno start_locb < Location.Bound.lineno end_locb then
	state.st_indent <- Location.Bound.column end_locb

let stacked_column state =
    match state.st_indent_stack with
    | [] -> -1
    | (col, _) :: _ -> col

let current_column state =
    if LStream.peek state.st_stream == None then -1 else
    let locb = LStream.locbound state.st_stream in
    Location.Bound.column locb

let push_end_token state loc =
    let col = Location.Bound.column (Location.lbound loc) in
    state.st_indent_stack <- (col, loc) :: state.st_indent_stack

let pop_token state =
    match state.st_stash with
    | Some (_, loc) as res ->
	state.st_stash <- None;
	if dlog_en then dlogf ~loc "Returning stashed token.";
	res
    | None ->
	if state.st_continued then begin
	    if dlog_en then
		dlogf ~loc:(Location.at (LStream.locbound state.st_stream))
		      "Continued block.";
	    None
	end else
	begin match state.st_indent_stack with
	| [] -> None
	| (col, loc) :: st_indent_stack' ->
	    if current_column state < col
	    then begin
		if dlog_en then
		    dlogf ~loc:(Location.at (LStream.locbound state.st_stream))
			"Inserting END/%d implied by BEGIN at %s"
			(List.length state.st_indent_stack)
			(Location.to_string loc);
		state.st_indent_stack <- st_indent_stack';
		Some (Grammar.END, loc)
	    end
	    else None
	end

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

let alias_operator state (Cidr (loc_orig, op_orig)) (Cidr (_, op)) =
    let op_orig' = UString_sequence.create (idr_to_ustring op_orig) in
    let op' = UString_sequence.create (idr_to_ustring op) in
    let ti =
	match UString_trie.find op_orig' state.st_keywords with
	| Some (Ti_operator (_, ok)) ->
	    Ti_operator (ok.Opkind.ok_create (op, []), ok)
	| Some (Ti_keyword _) ->
	    raise (Error_at (loc_orig, "Cannot alias keyword."))
	| None ->
	    raise (Error_at (loc_orig, "Cannot alias undefined operator.")) in
    if dlog_en then
	dlogf "Aliasing operator %s as %s."
	      (idr_to_string op) (idr_to_string op_orig);
    state.st_keywords <- UString_trie.add op' ti state.st_keywords

let skip_hspace state =
    LStream.skip_while UChar.is_hspace state.st_stream;
    begin match LStream.peek state.st_stream with
    | None -> false
    | Some ch -> ch != UChar.ch_nl
    end

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

let lexopen state = function
    | Ctrm_where (_, defs) -> List.iter
	begin function
	    | Cdef_lex (loc, okname, ops) ->
		let ok = Opkind.of_string okname in
		List.iter (declare_operator state ok) ops
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

let scan_keyword state =
    match triescan state state.st_keywords with
    | None ->
	(* Special handling of ".<space>". *)
	let loc_lb = LStream.locbound state.st_stream in
	begin match LStream.peek_n 2 state.st_stream with
	| [ch0; ch1] when UChar.code ch0 = 0x2e && UChar.is_space ch1 ->
	    LStream.skip state.st_stream;
	    let loc_ub = LStream.locbound state.st_stream in
	    Some (Grammar.DOT, Location.between loc_lb loc_ub)
	| _ -> None
	end
    | Some (ti, tokstr, loc) ->
	let (tok, _) =
	    match ti with
	    | Ti_keyword (Grammar.LEX, _) -> scan_lexdef state loc
	    | Ti_keyword (tok, _) | Ti_operator (tok, _) -> (tok, loc) in
	let loc_lb = Location.lbound loc in
	let lk = tokeninfo_lexkind ti in
	if state.st_continued
	     || lk = Opkind.Lex_intro
		&& stacked_column state <> Location.Bound.column loc_lb then
	  begin
	    state.st_continued <- lk = Opkind.Lex_continued;
	    let loc_begin = Location.between loc_lb loc_lb in
	    assert (state.st_stash = None);
	    state.st_stash <- Some (tok, loc);
	    push_end_token state loc_begin;
	    if dlog_en then
		dlogf ~loc:loc_begin "Inserting BEGIN/%d"
				     (List.length state.st_indent_stack);
	    Some (Grammar.BEGIN, loc_begin)
	  end
	else
	  begin
	    state.st_continued <- lk = Opkind.Lex_continued;
	    if dlog_en then dlogf ~loc "Scanned keyword.";
	    Some (tok, loc)
	  end

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
    idr_of_ustring (UString.Buf.contents buf)

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
    LStream.skip state.st_stream;
    let rec next () =
	match LStream.pop state.st_stream with
	| None -> raise Grammar.Error
	| Some ch ->
	    if ch = UChar.of_char '\\' then
		begin match LStream.pop state.st_stream with
		| None -> raise Grammar.Error
		| Some ch' ->
		    try
			let ch'' = UChar_map.find ch' escape_map in
			UString.Buf.add_char buf ch'';
			next ()
		    with Not_found -> raise Grammar.Error
		end
	    else if ch <> UChar.of_char '"' then begin
		UString.Buf.add_char buf ch;
		next ()
	    end in
    next ();
    Lit_string (UString.Buf.contents buf)

let scan_int have_sign base state =
    let rec f accu =
	let ch = Option.default UChar.ch_space (LStream.peek state.st_stream) in
	if not (UChar.is_idrchr ch) then accu else
	let code = UChar.code ch in
	let value =
	    if 0x30 <= code && code <= 0x39 then code - 0x30 else
	    if 0x61 <= code && code <= 0x66 then code - 0x61 + 10 else
	    raise Grammar.Error in
	LStream.skip state.st_stream;
	f (value + base * accu) in
    let n = (f 0) in
    Lit_int (if have_sign then -n else n)

let scan_literal state =
    let found lit =
	Some (Grammar.LITERAL lit) in
    let chcode_at n =
	match LStream.peek_at n state.st_stream with
	| None -> 0
	| Some ch -> UChar.code ch in
    let (n_sign, have_sign) =
	if chcode_at 0 = 0x2d then (1, true) else (0, false) in
    match chcode_at n_sign with
    | 0x22 ->
	if have_sign then None else found (scan_string_literal state)
    | 0x30 ->
	LStream.skip_n n_sign state.st_stream;
	begin match chcode_at 1 with
	| 0x62 (* 0b *) -> found (scan_int have_sign  2 state)
	| 0x6f (* 0o *) -> found (scan_int have_sign  8 state)
	| 0x78 (* 0x *) -> found (scan_int have_sign 16 state)
	| _ -> found (scan_int have_sign 10 state)
	end
    | ch0 ->
	LStream.skip_n n_sign state.st_stream;
	if 0x30 <= ch0 && ch0 <= 0x39 then
	    if chcode_at 1 = 0x27 (* ' *) then None else
	    found (scan_int have_sign 10 state)
	else None

let fixed_scanners = [pop_token; scan_keyword]
let default_scanners = [scan_literal; scan_identifier]

let default_state_template = {
    st_stream = LStream.null;
    st_indent = -1;
    st_indent_stack = [];
    st_stash = None;
    st_continued = false;
    st_keywords = initial_keywords;
    st_lookahead = initial_lookahead;
    st_last_location = Location.dummy;
    st_scanners = default_scanners;
}
let create_from_lstream st_stream =
    {default_state_template with st_stream = st_stream}
let create_from_file path =
    create_from_lstream (LStream.open_in path)

let lexer state () =
    skip_space state;
    match List.find_image (fun f -> f state) fixed_scanners with
    | Some (tok, loc) ->
	state.st_last_location <- loc;
	(tok, loc)
    | None ->
	let loc_lb = LStream.locbound state.st_stream in
	let tok =
	    match List.find_image (fun f -> f state) state.st_scanners with
	    | Some x -> x
	    | None ->
		state.st_last_location <- Location.at loc_lb;
		raise Grammar.Error
	    in
	let loc_ub = LStream.locbound state.st_stream in
	let loc = Location.between loc_lb loc_ub in
	state.st_last_location <- loc;
	if dlog_en then dlogf ~loc "Scanned token.";
	(tok, loc)
