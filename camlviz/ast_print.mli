(* Copyright 2011  Petter Urkedal
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

open Ast_types

val print_avar : Formatter.t -> avar -> unit
val print_apath : Formatter.t -> apath -> unit
val print_atyp : Formatter.t -> int -> atyp -> unit
val print_apat : Formatter.t -> int -> apat -> unit
val print_aval : Formatter.t -> int -> aval -> unit

val atyp_to_string : atyp -> string
val apat_to_string : apat -> string
val aval_to_string : aval -> string
