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
 * along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
 *)

let rtok_token (token, loc) = token
let rtok_bpos  (token, loc) =
    Location.Bound.to_lexing_position (Location.lbound loc)
let rtok_epos  (token, loc) =
    Location.Bound.to_lexing_position (Location.ubound loc)

let grammar_main =
    MenhirLib.Convert.traditional2revised
	rtok_token rtok_bpos rtok_epos
	Grammar.main

let parse_file path =
    let state = Lexer.create_from_file path in
    let lexer = Lexer.lexer state in
    try
	Some (grammar_main lexer)
    with Grammar.Error ->
	output_string stderr (Location.to_string (Lexer.last_location state));
	output_string stderr ": Syntax error.\n";
	None
