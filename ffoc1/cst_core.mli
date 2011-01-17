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
 * along with Fform/OC.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Unicode
open Cst_types

val idr_of_string : string -> idr

val idr_to_string : idr -> string

val idr_of_ustring : UString.t -> idr

val idr_to_ustring : idr -> UString.t

(** Add a unary operator prefix. *)
val idr_1o : idr -> idr
val idr_1o_c : string -> idr
val idr_1o_symbol : idr -> string

(** Add a binary operator prefix. *)
val idr_2o : idr -> idr
val idr_2o_c : string -> idr
val idr_2o_symbol : idr -> string

val idr_1b : idr -> idr -> idr
val idr_1b_c : string -> string -> idr

val idr_1q : idr -> idr
val idr_1q_c : string -> idr
val idr_1q_symbol : idr -> string

val cidr_loc : cidr -> loc
val cidr_to_idr : cidr -> idr
val cidr_to_string : cidr -> string

module Idr_set : Set.S with type elt = idr
module Idr_map : Map.S with type key = idr

val ctrm_loc : ctrm -> loc

val application_depth : int -> idr -> ctrm -> int

val print : Formatter.t -> ctrm -> unit
val ctrm_to_string : ctrm -> string
val cdef_to_string : cdef -> string


(* Specific Symbols *)
val idr_2o_arrow : idr
val idr_1o_not : idr
val idr_2o_and : idr
val idr_2o_or : idr
val cidr_is_2o_colon : cidr -> bool
val cidr_is_2o_comma : cidr -> bool
val cidr_is_2o_arrow : cidr -> bool
val cidr_is_2o_eq : cidr -> bool
val cidr_is_1q_functor : cidr -> bool
