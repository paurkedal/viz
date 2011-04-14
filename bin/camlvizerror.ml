(* Copyright 2011  Petter Urkedal
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
open Camlviz.FfPervasives
open Camlviz.Unicode

let rx = Str.regexp

let uchar_re = rx "[uU]\\([0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\)"
let op_re = rx "\\bop\\([0-2]\\)_"

let demangle =
    let replace_uchar ln =
	let codestr = Str.matched_group 1 ln in
	let code = Scanf.sscanf codestr "%x" (fun n -> n) in
	UChar.to_utf8 (UChar.chr code) in
    Str.global_replace op_re "\\1'" *>
    Str.global_substitute uchar_re replace_uchar

let loc0_re =
    rx ".*loc: \\[\"\\([^\"]+\\)\": +\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)"
let show_error ich och mlpath mlline msg =
    let mlch = open_in mlpath in
    let match_locline locln =
	if not (Str.string_match loc0_re locln 0) then false else
	let vzpath = Str.matched_group 1 locln in
	let vzline = Str.matched_group 2 locln in
	let scol = Str.matched_group 3 locln in
	let ecol = Str.matched_group 4 locln in
	fprintf och "%s:%s,%s-%s: %s\n" vzpath vzline scol ecol (demangle msg);
	true in
    begin try
	for i = 3 to mlline do ignore (input_line mlch) done;
	if not begin
	    if mlline < 2 then match_locline (input_line mlch) else
	    let locln1 = input_line mlch in
	    let locln0 = input_line mlch in
	    match_locline locln0 || match_locline locln1
	end then begin
	    flush och;
	    eprintf "camlvizerror: Did not find a location at %s:%d.\n"
		    mlpath mlline;
	    flush stderr;
	    fprintf och "%s\n" (demangle msg)
	end
    with End_of_file ->
	flush och;
	eprintf "camlvizerror: Failed to skip to line %d in %s.\n"
		mlline mlpath;
	flush stderr
    end;
    close_in mlch

let error_re =
    rx "File \"\\([^\"]+\\)\", line \\([0-9]+\\), characters [0-9-]+:"
let process_errors ich och =
    try while true do
	let ln = input_line ich in
	if Str.string_match error_re ln 0 then begin
	    let mlpath = Str.matched_group 1 ln in
	    let mlline = int_of_string (Str.matched_group 2 ln) in
	    let msg = input_line ich in
	    show_error ich och mlpath mlline msg
	end else
	fprintf och "%s\n" ln
    done with End_of_file -> ()

let () =
    Arg.parse [] (fun _ -> raise (Arg.Bad "No arguments expected."))
	"camlvizerror -- Filter error 'ocamlc' and 'ocamlopt' error messages\n\
	\                from code generated with 'camlvizpp --add-locations'.";
    process_errors stdin stdout
