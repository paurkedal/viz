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

open FfPervasives

type tag = [`Error | `Keyword | `Label | `Literal | `Name | `Operator]

type t = {
    fo_buf : Buffer.t;
    mutable fo_indent : int;
    mutable fo_column : int;
    mutable fo_prev_char : char;
    fo_enter : t -> tag -> unit;
    fo_leave : t -> tag -> unit;
}

let contents fo = Buffer.contents fo.fo_buf

let add_indent fo n = fo.fo_indent <- fo.fo_indent + n

let enter_indent fo = add_indent fo 4
let leave_indent fo = add_indent fo (- 4)

let do_indent fo =
    let rec indent_1x n =
	if n > 0 then begin
	    Buffer.add_char fo.fo_buf ' ';
	    indent_1x (n - 1)
	end in
    let rec indent_8x n =
	if n >= 8 then begin
	    Buffer.add_char fo.fo_buf '\t';
	    indent_8x (n - 8)
	end
	else indent_1x n in
    indent_8x fo.fo_indent;
    fo.fo_column <- fo.fo_indent

let create ?(indent = 0) ?enter ?leave () =
    {
	fo_buf = Buffer.create 80;
	fo_indent = indent;
	fo_column = 0;
	fo_prev_char = ' ';
	fo_enter = Option.default (fun _ _ -> ()) enter;
	fo_leave = Option.default (fun _ _ -> ()) leave;
    }

let put_char fo ch =
    Buffer.add_char fo.fo_buf ch;
    fo.fo_column <- fo.fo_column + 1;
    fo.fo_prev_char <- ch

let space fo =
    if not (Char.is_space fo.fo_prev_char) then
	put_char fo ' '

let newline fo =
    if fo.fo_prev_char <> '\n' then
	put_char fo '\n';
    do_indent fo

let put_string fo s =
    let n = String.length s in
    if n > 0 then begin
	Buffer.add_string fo.fo_buf s;
	fo.fo_column <- fo.fo_column + n;
	fo.fo_prev_char <- String.get s (n - 1)
    end

let put fo tag s =
    fo.fo_enter fo tag;
    put_string fo s;
    fo.fo_leave fo tag

let put_op fo op =
    space fo;
    put fo `Operator op;
    space fo

let put_kw fo name =
    space fo;
    put fo `Keyword name;
    space fo
