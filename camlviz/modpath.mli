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

open Leaf_types
open Sexplib

type t

val empty : t
val is_empty : t -> bool

val atom : idr -> t
val is_atom : t -> bool

val cat_last : idr -> t -> t
val cut_last : t -> (idr * t) option
val last_e : t -> idr
val strip_last_e : t -> t

val length : t -> int
val nth_last : int -> t -> idr option

val has_suffix : t -> t -> bool
val has_prefix : t -> t -> bool

val cat : t -> t -> t
val strip_suffix : t -> t -> t option
val strip_suffix_e : t -> t -> t
val strip_prefix : t -> t -> t option
val strip_prefix_e : t -> t -> t
val strip_common_prefix : t -> t -> t

val fold : (idr -> 'a -> 'a) -> t -> 'a -> 'a
val rfold : (idr -> 'a -> 'a) -> t -> 'a -> 'a

val iter : (idr -> unit) -> t -> unit
val riter : (idr -> unit) -> t -> unit

val compare : t -> t -> int

val of_string : string -> t
val to_string : t -> string
val of_idr_list : idr list -> t
val to_idr_list : t -> idr list
val of_string_list : string list -> t
val to_string_list : t -> string list

val t_of_sexp : Sexp.t -> t
val sexp_of_t : t -> Sexp.t
