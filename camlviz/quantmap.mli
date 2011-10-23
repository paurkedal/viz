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

type t

val empty : t

val card : t -> int

val load : roots: string list -> string -> t -> t

val load_all : roots: string list -> t -> t

val add : atyp -> Modpath.t -> t -> t

val find : atyp -> t -> Modpath.t option

val open_module : Modpath.t -> t -> t

val filter_by_path : Modpath.t -> t -> t

val fold : (atyp -> Modpath.t -> 'a -> 'a) -> t -> 'a -> 'a
