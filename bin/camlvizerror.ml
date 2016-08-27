(* Copyright 2011--2016  Petter A. Urkedal
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

open Printf
open Camlviz
open Camlviz.FfPervasives
open Camlviz.Unicode

let rx = Str.regexp

let op_re = rx "\\bop\\([0-2]\\)_"
let subst1_re = rx "'z__\\(U03\\)"
let strip_re = rx "\\bz__"
let uchar_re = rx "[uU]\\([0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\)"

let demangle =
    let replace_uchar ln =
	let codestr = Str.matched_group 1 ln in
	let code = Scanf.sscanf codestr "%x" (fun n -> n) in
	UChar.to_utf8 (UChar.chr code) in
    Str.global_replace op_re "\\1'" *>
    Str.global_replace subst1_re "\\1" *>
    Str.global_replace strip_re "" *>
    Str.global_substitute uchar_re replace_uchar

let loc0_re =
    rx ".*loc: \\[\"\\([^\"]+\\)\": +\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)"
let show_error ich och mlpath mlline msg =
    let mlch = open_in mlpath in
    let match_locline locln =
	if not (Str.string_match loc0_re locln 0) then false else
	let vzpath = Str.matched_group 1 locln in
	let vzline = int_of_string (Str.matched_group 2 locln) in
	let scol = int_of_string (Str.matched_group 3 locln) in
	let ecol = int_of_string (Str.matched_group 4 locln) in
	let msg =
	    if String.starts_with "Error: " msg then String.after 7 msg else
	    msg in
	fprintf och "%s:%s,%s-%s: %s\n" vzpath
	    (Textloc.string_of_lineno vzline)
	    (Textloc.string_of_colno scol)
	    (Textloc.string_of_colno ecol)
	    (demangle msg);
	true in
    begin try
	for i = 4 to mlline do ignore (input_line mlch) done;
	if not begin
	    let locln2 = if mlline < 3 then "" else input_line mlch in
	    let locln1 = if mlline < 2 then "" else input_line mlch in
	    let locln0 = input_line mlch in
	    match_locline locln0 || match_locline locln1 || match_locline locln2
	end then begin
	    flush och;
	    if msg <> "Error: Preprocessor error" then begin
		eprintf "camlvizerror: Did not find a location at %s:%d.\n"
			mlpath mlline;
		flush stderr;
		fprintf och "%s\n" (demangle msg)
	    end
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
let ingore_re =
    rx "Warning 25: bad style, all clauses in this pattern-matching are \
	guarded\\."
let process_errors ich och =
    let err = ref 0 in
    begin try while true do
	let ln = input_line ich in
	if Str.string_match error_re ln 0 then begin
	    let mlpath = Str.matched_group 1 ln in
	    let mlline = int_of_string (Str.matched_group 2 ln) in
	    let msg = input_line ich in
	    err := 1;
	    if Str.string_match ingore_re msg 0 then () else
	    try
		show_error ich och mlpath mlline msg
	    with Sys_error _ ->
		fprintf och "%s\n%s\n" ln msg
	end else
	fprintf och "%s\n" (demangle ln)
    done with End_of_file -> () end;
    !err

let () =
    Arg.parse [] (fun _ -> raise (Arg.Bad "No arguments expected."))
	"camlvizerror -- Filter error 'ocamlc' and 'ocamlopt' error messages\n\
	\                from code generated with 'camlvizpp --add-locations'.";
    exit (process_errors stdin stdout)
