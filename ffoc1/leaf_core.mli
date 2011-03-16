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

open Leaf_types
open Unicode

val lit_to_string : lit -> string

val idr_of_string : string -> idr

val idr_to_string : idr -> string

val idr_of_ustring : UString.t -> idr

val idr_to_ustring : idr -> UString.t

val idr_0o : idr -> idr
val idr_0o_c : string -> idr
val idr_0o_symbol : idr -> string

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
val idr_2b : idr -> idr -> idr
val idr_2b_c : string -> string -> idr

val idr_1q : idr -> idr
val idr_1q_c : string -> idr
val idr_1q_symbol : idr -> string

module Idr_set : Set.S with type elt = idr
module Idr_map : Map.S with type key = idr
