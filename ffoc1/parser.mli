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

(** Top-Level Entry Point for Parsing Files *)

open Cst_types

val locate_source :
    ?exts: string list -> ?strip_ext: bool ->
    ?topdir: string -> roots: string list ->
    string -> string

val parse_file :
    ?exts: string list -> roots: string list -> string -> ctrm option

val find_and_parse_file :
    ?exts: string list -> roots: string list -> string -> ctrm option
