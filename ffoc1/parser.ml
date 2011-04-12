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

open Printf
open FfPervasives

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

let prune_path =
    String.strip_suffix "_FFIC" *> String.strip_suffix "_"

let locate_source ?(exts = [".vz"; ".viz"; ".ff"; ".ml"; ".mlpack"])
		  ?(strip_ext = false)
		  ?topdir ~roots rel_path_sans_ext =
    let rec check_roots = function
	| [] -> raise Not_found
	| root :: roots ->
	    let path_sans_ext = Filename.concat root rel_path_sans_ext in
	    let rec check_exts = function
		| [] -> check_roots roots
		| ext :: exts ->
		    let path = path_sans_ext ^ ext in
		    let path' = prune_path path_sans_ext ^ ext in
		    let path' =
			match topdir with
			| None -> path'
			| Some topdir -> Filename.concat topdir path' in
		    if Sys.file_exists path' then
			if strip_ext then path_sans_ext else path
		    else check_exts exts in
	    check_exts exts in
    check_roots roots

let load_stdlex root_paths =
    try
	let path = locate_source root_paths "stdlex" in
	let state = Lexer.create_from_file path in
	let lexer = Lexer.lexer state in
	grammar_main lexer
    with Not_found ->
	print_error Location.dummy "Cannot find the stdlex structure.";
	raise Not_found

let parse_file ?exts ~roots path =
    let state = Lexer.create_from_file path in
    Lexer.lexopen state (load_stdlex roots);
    let lexer = Lexer.lexer state in
    try Some (grammar_main lexer) with
    | Diag.Error_at (loc, msg) ->
	print_error loc msg;
	None
    | Grammar.Error ->
	print_error (Lexer.last_location state) "Syntax error.";
	None

let find_and_parse_file ?exts ~roots path =
    try
	let path = locate_source ?exts ~roots path in
	parse_file ?exts ~roots path
    with Not_found ->
	print_error Location.dummy (sprintf "Could not find %s." path);
	None
