(* Copyright 2010  Petter Urkedal
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

exception Error_at of Location.t * string

let dlog_en = Diag.dlog_en_for "Fform.Lexer"
let dlogf fmt ?loc = Diag.dlogf_for "Fform.Lexer" ?loc fmt

type kwinfo = {
    ki_mktoken : UString.t -> Grammar.token;
    ki_is_intro : bool;
    ki_continued : bool;
}

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
    mutable st_keywords : kwinfo UString_trie.t;
    mutable st_operators : Opkind.t UString_trie.t;

    mutable st_lookahead : int;
    mutable st_last_location : Location.t;
    mutable st_scanners : (state -> Grammar.token option) list;
}

let last_location state = state.st_last_location

let mktoken_arr =
    let a = Array.create Opkind.maxp_id
	(fun _ -> raise (Failure "Invalid precedence")) in
    List.iter (fun (p, mktoken) -> Array.set a p mktoken) [
	Opkind.mixfix_quantifier.Opkind.ok_id, (fun a -> Grammar.QUANTIFIER a);
	Opkind.preinfix_logic.(0).Opkind.ok_id, (fun a -> Grammar.LOGIC0 a);
	Opkind.preinfix_logic.(1).Opkind.ok_id, (fun a -> Grammar.LOGIC1 a);
	Opkind.preinfix_logic.(2).Opkind.ok_id, (fun a -> Grammar.LOGIC2 a);
	Opkind.preinfix_logic.(3).Opkind.ok_id, (fun a -> Grammar.LOGIC3 a);
	Opkind.preinfix_logic.(4).Opkind.ok_id, (fun a -> Grammar.LOGIC4 a);
	Opkind.preinfix_logic.(5).Opkind.ok_id, (fun a -> Grammar.LOGIC5 a);
	Opkind.preinfix_logic.(6).Opkind.ok_id, (fun a -> Grammar.LOGIC6 a);
	Opkind.preinfix_logic.(7).Opkind.ok_id, (fun a -> Grammar.LOGIC7 a);
	Opkind.preinfix_logic.(8).Opkind.ok_id, (fun a -> Grammar.LOGIC8 a);
	Opkind.transfix_relation.Opkind.ok_id, (fun a -> Grammar.RELATION a);
	Opkind.preinfix_arith.(0).Opkind.ok_id, (fun a -> Grammar.ARITH0 a);
	Opkind.preinfix_arith.(1).Opkind.ok_id, (fun a -> Grammar.ARITH1 a);
	Opkind.preinfix_arith.(2).Opkind.ok_id, (fun a -> Grammar.ARITH2 a);
	Opkind.preinfix_arith.(3).Opkind.ok_id, (fun a -> Grammar.ARITH3 a);
	Opkind.preinfix_arith.(4).Opkind.ok_id, (fun a -> Grammar.ARITH4 a);
	Opkind.preinfix_arith.(5).Opkind.ok_id, (fun a -> Grammar.ARITH5 a);
	Opkind.preinfix_arith.(6).Opkind.ok_id, (fun a -> Grammar.ARITH6 a);
	Opkind.preinfix_arith.(7).Opkind.ok_id, (fun a -> Grammar.ARITH7 a);
	Opkind.preinfix_arith.(8).Opkind.ok_id, (fun a -> Grammar.ARITH8 a);
	Opkind.preinfix_arith.(9).Opkind.ok_id, (fun a -> Grammar.ARITH9 a);
	Opkind.suffix_arith.(0).Opkind.ok_id, (fun a -> Grammar.ARITH0_S a);
	Opkind.suffix_arith.(1).Opkind.ok_id, (fun a -> Grammar.ARITH1_S a);
	Opkind.suffix_arith.(2).Opkind.ok_id, (fun a -> Grammar.ARITH2_S a);
	Opkind.suffix_arith.(3).Opkind.ok_id, (fun a -> Grammar.ARITH3_S a);
	Opkind.suffix_arith.(4).Opkind.ok_id, (fun a -> Grammar.ARITH4_S a);
	Opkind.suffix_arith.(5).Opkind.ok_id, (fun a -> Grammar.ARITH5_S a);
	Opkind.suffix_arith.(6).Opkind.ok_id, (fun a -> Grammar.ARITH6_S a);
	Opkind.suffix_arith.(7).Opkind.ok_id, (fun a -> Grammar.ARITH7_S a);
	Opkind.suffix_arith.(8).Opkind.ok_id, (fun a -> Grammar.ARITH8_S a);
	Opkind.suffix_arith.(9).Opkind.ok_id, (fun a -> Grammar.ARITH9_S a);
	Opkind.infix_script.(0).Opkind.ok_id, (fun a -> Grammar.SCRIPT0_I a);
	Opkind.infix_script.(1).Opkind.ok_id, (fun a -> Grammar.SCRIPT1_I a);
	Opkind.infix_script.(2).Opkind.ok_id, (fun a -> Grammar.SCRIPT2_I a);
	Opkind.prefix_script.(0).Opkind.ok_id, (fun a -> Grammar.SCRIPT0_P a);
	Opkind.prefix_script.(1).Opkind.ok_id, (fun a -> Grammar.SCRIPT1_P a);
	Opkind.prefix_script.(2).Opkind.ok_id, (fun a -> Grammar.SCRIPT2_P a);
	Opkind.suffix_script.(0).Opkind.ok_id, (fun a -> Grammar.SCRIPT0_S a);
	Opkind.suffix_script.(1).Opkind.ok_id, (fun a -> Grammar.SCRIPT1_S a);
	Opkind.suffix_script.(2).Opkind.ok_id, (fun a -> Grammar.SCRIPT2_S a);
    ];
    a

let initial_intro_keywords = [
    "open",	Grammar.OPEN;
    "include",	Grammar.INCLUDE;
    "in",	Grammar.IN;
    "sig",	Grammar.SIG;
    "type",	Grammar.TYPE;
    "let",	Grammar.LET;
    "val",	Grammar.VAL;
    "inj",	Grammar.INJ;
    "be",	Grammar.BE;
    "do",	Grammar.DO;
    "raise",	Grammar.RAISE;
    "upon",	Grammar.UPON;
    "lex",	Grammar.LEX;
    "lexalias",	Grammar.LEXALIAS;
    "leximport",Grammar.LEXIMPORT;
    "__notation__",Grammar.NOTATION;
    "if",	Grammar.IF;
    "else",	Grammar.ELSE;
    "otherwise",Grammar.OTHERWISE;
    "at",	Grammar.AT;
]
let initial_plain_keywords = [
    "(",	Grammar.LPAREN;
    ")",	Grammar.RPAREN;
    "↦",	Grammar.MAPSTO;
    "/>",	Grammar.MAPSTO;
    "true",	Grammar.LITERAL (Cst.Lit_bool true);
    "false",	Grammar.LITERAL (Cst.Lit_bool false);
]
let initial_continued_keywords = [
    "where",	Grammar.WHERE;
    "with",	Grammar.WITH;
    "what",	Grammar.WHAT;
    "which",	Grammar.WHICH;
]
let initial_lookahead = 40

let initial_keywords =
    let addkw is_intro is_continued (name, token) =
	let kwinfo = {
	    ki_mktoken = (fun _ -> token);
	    ki_is_intro = is_intro;
	    ki_continued = is_continued;
	} in
	UString_trie.add_utf8 name kwinfo in
    List.fold (addkw true  false) initial_intro_keywords @<
    List.fold (addkw false false) initial_plain_keywords @<
    List.fold (addkw false true)  initial_continued_keywords @<
    UString_trie.empty

let initial_operators = UString_trie.empty

let rec skip_line state =
    match LStream.pop state.st_stream with
    | None -> ()
    | Some ch -> if int_of_uchar ch != 0xa then skip_line state

let skip_space state =
    (* Skip spaces and comments, update indentation. *)
    let start_locb = LStream.locbound state.st_stream in
    let rec loop () =
	match LStream.peek state.st_stream with
	| None -> ()
	| Some ch ->
	    if UChar.is_space ch then
		begin
		    LStream.skip state.st_stream;
		    loop ()
		end else
	    if ch = UChar.ch_dash
		    && Option.exists ((=) UChar.ch_dash)
				     (LStream.peek_at 1 state.st_stream) then
		begin
		    skip_line state;
		    loop ()
		end in
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

let declare_operator state ok (Cst.Cidr (_, op)) =
    let op' = UString_sequence.create (Cst.idr_to_ustring op) in
    if dlog_en then
	dlogf "Declaring operator %s at %s." (Cst.idr_to_string op)
	      (Opkind.to_string ok);
    state.st_operators <- UString_trie.add op' ok state.st_operators

let scan_lexdef state lex_loc =
    skip_space state;
    let okname, okname_loc =
	LStream.scan_while (not *< UChar.is_space) state.st_stream in
    let loc_lb = LStream.locbound state.st_stream in
    let ops_r = ref [] in
    while
	LStream.skip_while UChar.is_hspace state.st_stream;
	begin match LStream.peek state.st_stream with
	| None -> false
	| Some ch -> ch != UChar.ch_nl
	end
    do
	let opname, opname_loc =
	    LStream.scan_while (not *< UChar.is_space) state.st_stream in
	let op = Cst.Cidr (opname_loc, Cst.idr_of_ustring opname) in
	ops_r := op :: !ops_r
    done;
    let ops = List.rev !ops_r in
    let opkind =
	try Opkind.of_string (UString.to_utf8 okname)
	with Opkind.Domain_error ->
	    raise (Error_at (okname_loc, "Not an operator kind.")) in
    let loc_ub = LStream.locbound state.st_stream in
    let loc = Location.between loc_lb loc_ub in
    let lexdef = Cst.Dec_lex (loc, opkind, ops) in
    List.iter (declare_operator state opkind) ops;
    (Grammar.PREPARED_DEF lexdef, loc)

let lexopen state = function
    | Cst.Trm_where (_, defs) -> List.iter
	begin function
	    | Cst.Dec_lex (loc, ok, ops) ->
		List.iter (declare_operator state ok) ops
	    | _ -> ()
	end
	defs
    | trm ->
	let loc = Cst.trm_location trm in
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
	(* Special handling of ".<identifier>" and ".<space>". *)
	begin match LStream.peek_n 2 state.st_stream with
	| [ch0; ch1] when UChar.code ch0 = 0x2e ->
	    let loc_lb = LStream.locbound state.st_stream in
	    LStream.skip state.st_stream;
	    if UChar.is_idrchr ch1 then
		let (name, loc) =
		    LStream.scan_while UChar.is_idrchr state.st_stream in
		let idr = Cst.idr_of_ustring name in
		let loc' = Location.between loc_lb (Location.ubound loc) in
		Some (Grammar.PROJECT idr, loc')
	    else if UChar.is_space ch1 then
		let loc_ub = LStream.locbound state.st_stream in
		Some (Grammar.DOT, Location.between loc_lb loc_ub)
	    else
		None
	| _ -> None
	end
    | Some (kwinfo, tokstr, loc) ->
	let (tok, _) =
	    match kwinfo.ki_mktoken tokstr with
	    | Grammar.LEX -> scan_lexdef state loc
	    | tok -> (tok, loc) in
	let loc_lb = Location.lbound loc in
	if state.st_continued
	     || kwinfo.ki_is_intro
		&& stacked_column state <> Location.Bound.column loc_lb then
	  begin
	    state.st_continued <- kwinfo.ki_continued;
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
	    state.st_continued <- kwinfo.ki_continued;
	    if dlog_en then dlogf ~loc "Scanned keyword.";
	    Some (tok, loc)
	  end

let scan_operator state =
    match triescan state state.st_operators with
    | None -> None
    | Some (opkind, tokstr, loc) ->
	let mktoken = mktoken_arr.(opkind.Opkind.ok_id) in
	if dlog_en then dlogf ~loc "Scanned operator.";
	Some (mktoken (Cst.idr_of_ustring tokstr), loc)

let scan_identifier state =
    let buf = UString.Buf.create 8 in
    let finish () =
	let s = UString.Buf.contents buf in
	let n = UString.length s in
	match LStream.peek_code state.st_stream with
	| 0x25 (* % *) ->
	    LStream.skip state.st_stream;
	    let idr = Cst.idr_of_ustring s in
	    Grammar.HINTED_IDENTIFIER (idr, Cst.Ih_inj)
	| _ ->
	match int_of_uchar (UString.get s (n - 1)) with
	| 0x5f (* _ *) when n > 1 ->
	    let idr = Cst.idr_of_ustring (UString.sub s 0 (n - 1)) in
	    Grammar.HINTED_IDENTIFIER (idr, Cst.Ih_univ)
	| _ ->
	    let idr = Cst.idr_of_ustring s in
	    Grammar.IDENTIFIER idr in
    let rec scan prev_ch =
	LStream.skip state.st_stream;
	match LStream.peek state.st_stream with
	| None -> finish ()
	| Some next_ch ->
	    if UChar.are_tied prev_ch next_ch then begin
		UString.Buf.add_char buf next_ch;
		scan next_ch
	    end else finish () in
    match LStream.peek state.st_stream with
    | None -> Some Grammar.EOF
    | Some ch ->
	if UChar.is_idrchr ch then begin
	    UString.Buf.add_char buf ch;
	    Some (scan ch)
	end else None

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
    Cst.Lit_string (UString.Buf.contents buf)

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
    Cst.Lit_int (if have_sign then -n else n)

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
	    found (scan_int have_sign 10 state)
	else None

let fixed_scanners = [pop_token; scan_keyword; scan_operator]
let default_scanners = [scan_literal; scan_identifier]

let default_state_template = {
    st_stream = LStream.null;
    st_indent = -1;
    st_indent_stack = [];
    st_stash = None;
    st_continued = false;
    st_keywords = initial_keywords;
    st_operators = initial_operators;
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
