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

let dlog_en = Diag.dlog_en_for "Fform.Lexer"
let dlogf fmt ?loc = Diag.dlogf_for "Fform.Lexer" ?loc fmt

type kwinfo = {
    create_token : UString.t -> Grammar.token;
    is_intro : bool;
}

type state = {
    stream : LStream.t;
    mutable indent : int;
    mutable indent_stack : (int * Location.t) list;
    mutable stashed_token : (Grammar.token * Location.t) option;
    mutable keywords : kwinfo UString_trie.t;
    mutable lookahead : int;
    mutable last_location : Location.t;
    mutable scanners : (state -> Grammar.token option) list;
}

let last_location state = state.last_location

let initial_intro_keywords = [
    "using",	Grammar.USING;
    "using_lib",Grammar.USING_LIB;
    "import",	Grammar.IMPORT;
    "sig",	Grammar.SIG;
    "struct",	Grammar.STRUCT;
    "type",	Grammar.TYPE;
    "val",	Grammar.VAL;
    "inj",	Grammar.INJ;
    "is",	Grammar.IS;
    "do",	Grammar.DO;
    "given",	Grammar.GIVEN;
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
    "where",	Grammar.WHERE;
    "with",	Grammar.WITH;
    "what",	Grammar.WHAT;
    "(",	Grammar.LPAREN;
    ")",	Grammar.RPAREN;
    ":",	Grammar.COLON;
    ",",	Grammar.COMMA;
    "↦",	Grammar.MAPSTO;
    "/>",	Grammar.MAPSTO;
    ".",	Grammar.DOT;
]
let initial_lookahead = 40

let initial_keywords =
    let addkw is_intro (name, token) =
	let kwinfo = {
	    create_token = (fun _ -> token);
	    is_intro = is_intro;
	} in
	UString_trie.add_utf8 name kwinfo in
    List.fold (addkw true)  initial_intro_keywords @<
    List.fold (addkw false) initial_plain_keywords @<
    UString_trie.empty

let rec skip_line state =
    match LStream.pop state.stream with
    | None -> ()
    | Some ch -> if int_of_uchar ch != 0xa then skip_line state

let skip_space state =
    (* Skip spaces and comments, update indentation. *)
    let start_locb = LStream.locbound state.stream in
    while match LStream.peek state.stream with
    | None -> false
    | Some ch ->
	if ch = UChar.ch_dash
		&& Option.exists ((=) UChar.ch_dash)
				 (LStream.peek_at 1 state.stream) then
	    begin
		skip_line state;
		Option.exists UChar.is_space (LStream.peek state.stream)
	    end
	else
	    UChar.is_space ch
    do LStream.skip state.stream done;
    let end_locb = LStream.locbound state.stream in
    if Location.Bound.lineno start_locb < Location.Bound.lineno end_locb then
	state.indent <- Location.Bound.column end_locb

let stacked_column state =
    match state.indent_stack with
    | [] -> -1
    | (col, _) :: _ -> col

let current_column state =
    let locb = LStream.locbound state.stream in
    Location.Bound.column locb

let push_end_token state loc =
    let col = Location.Bound.column (Location.lbound loc) in
    state.indent_stack <- (col, loc) :: state.indent_stack

let pop_token state =
    match state.stashed_token with
    | Some (_, loc) as res ->
	state.stashed_token <- None;
	if dlog_en then dlogf ~loc "Returning stashed token.";
	res
    | None ->
	begin match state.indent_stack with
	| [] -> None
	| (col, loc) :: indent_stack' ->
	    if current_column state < col
	    then begin
		if dlog_en then
		    dlogf ~loc:(Location.at (LStream.locbound state.stream))
			"Inserting END implied by BEGIN at %s"
			(Location.to_string loc);
		state.indent_stack <- indent_stack';
		Some (Grammar.END, loc)
	    end
	    else None
	end

let scan_keyword state =
    let la = UString_sequence.create
			(LStream.peek_n state.lookahead state.stream) in
    let f la kwi_opt (len, last_ch, res) =
	let this_ch = Option.default UChar.ch_space (Sequence.peek la) in
	if UChar.are_tied last_ch this_ch
	then (len + 1, this_ch, res)
	else
	    let res' =
		match kwi_opt with
		| None -> res
		| Some kwi -> Some (len, kwi) in
	    (len + 1, this_ch, res') in
    match UString_trie.prefix_optfold f la state.keywords
				      (0, UChar.ch_space, None) with
    | (_, _, None) -> None
    | (_, _, Some (len, kwinfo)) ->
	let tok = kwinfo.create_token (UString.of_sequence_n len la) in
	let loc_lb = LStream.locbound state.stream in
	if kwinfo.is_intro && stacked_column state <> current_column state then
	  begin
	    LStream.skip_n len state.stream;
	    let loc_ub = LStream.locbound state.stream in
	    let loc_begin = Location.between loc_lb loc_lb in
	    let loc_kw = Location.between loc_lb loc_ub in
	    assert (state.stashed_token = None);
	    state.stashed_token <- Some (tok, loc_kw);
	    push_end_token state loc_begin;
	    if dlog_en then dlogf ~loc:loc_begin "Inserting BEGIN";
	    Some (Grammar.BEGIN, loc_begin)
	  end
	else
	  begin
	    LStream.skip_n len state.stream;
	    let loc_ub = LStream.locbound state.stream in
	    let loc = Location.between loc_lb loc_ub in
	    if dlog_en then dlogf ~loc "Matched keyword.";
	    Some (tok, loc)
	  end

let scan_identifier state =
    let buf = UString.Buf.create 8 in
    let finish () =
	let name = UString.Buf.contents buf in
	let tok = Grammar.IDENTIFIER (Input.idr_of_ustring name) in
	tok in
    let rec scan prev_ch =
	LStream.skip state.stream;
	match LStream.peek state.stream with
	| None -> finish ()
	| Some next_ch ->
	    UString.Buf.add_char buf next_ch;
	    if UChar.are_tied prev_ch next_ch then scan next_ch
	    else finish () in
    match LStream.peek state.stream with
    | None -> Some Grammar.EOF
    | Some ch ->
	if UChar.is_idrchr ch then begin
	    UString.Buf.add_char buf ch;
	    Some (scan ch)
	end else None

let fixed_scanners = [pop_token; scan_keyword]
let default_scanners = [scan_identifier]

let create_from_lstream stream =
    {
	stream = stream;
	indent = -1;
	indent_stack = [];
	stashed_token = None;
	keywords = initial_keywords;
	lookahead = initial_lookahead;
	last_location = Location.dummy;
	scanners = default_scanners;
    }
let create_from_file path = create_from_lstream (LStream.open_in path)

let lexer state () =
    skip_space state;
    match List.find_image (fun f -> f state) fixed_scanners with
    | Some (tok, loc) ->
	state.last_location <- loc;
	(tok, loc)
    | None ->
	let loc_lb = LStream.locbound state.stream in
	let tok =
	    match List.find_image (fun f -> f state) state.scanners with
	    | Some x -> x
	    | None ->
		state.last_location <- Location.at loc_lb;
		raise Grammar.Error
	    in
	let loc_ub = LStream.locbound state.stream in
	let loc = Location.between loc_lb loc_ub in
	state.last_location <- loc;
	if dlog_en then dlogf ~loc "Scanned token.";
	(tok, loc)
