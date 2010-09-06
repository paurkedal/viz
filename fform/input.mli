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

include module type of Input_types

val idr_of_string : string -> idr

val idr_to_string : idr -> string

val idr_of_ustring : UString.t -> idr

val idr_to_ustring : idr -> UString.t

(** Add a unary operator prefix. *)
val idr_1o : idr -> idr

(** Add a binary operator prefix. *)
val idr_2o : idr -> idr

val i_2o_comma : idr
val i_2o_arrow : idr
val i_2o_eq : idr
val i_2o_neq : idr

module Idr_set : Set.S with type elt = idr
module Idr_map : Map.S with type key = idr

val trm_location : trm -> loc

val trm_ref : loc -> string -> trm

val tuple_op : trm

val application_depth : int -> idr -> trm -> int

val print : Formatter.t -> trm -> unit
