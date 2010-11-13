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

open Printf

let rtok_token (token, loc) = token
let rtok_bpos  (token, loc) =
    Location.Bound.to_lexing_position (Location.lbound loc)
let rtok_epos  (token, loc) =
    Location.Bound.to_lexing_position (Location.ubound loc)

let grammar_main =
    MenhirLib.Convert.traditional2revised
	rtok_token rtok_bpos rtok_epos
	Grammar.main

let print_error loc msg =
    eprintf "%s: %s\n" (Location.to_string loc) msg

let stdlex =
    let state = Lexer.create_from_file "fflib/stdlex.ff" in
    let lexer = Lexer.lexer state in
    grammar_main lexer

let parse_file path =
    let state = Lexer.create_from_file path in
    Lexer.lexopen state stdlex;
    let lexer = Lexer.lexer state in
    try Some (grammar_main lexer) with
    | Lexer.Error_at (loc, msg) ->
	print_error loc msg;
	None
    | Grammar.Error ->
	print_error (Lexer.last_location state) "Syntax error.";
	None
