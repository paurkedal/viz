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

(** Top-Level Entry Point for Parsing Files *)

open Cst_types

val locate_source :
    ?exts: string list -> ?strip_ext: bool ->
    ?topdir: string -> roots: string list ->
    string -> string

val parse_string :
    ?locb : Location.Bound.t -> roots: string list -> string -> ctrm

val parse_file : roots: string list -> string -> ctrm

val find_and_parse_file :
    ?exts: string list -> roots: string list -> string -> ctrm
