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

(** Core Functions for the Concrete Syntax Tree *)

open Unicode
open Leaf_types
open Cst_types

val cidr_loc : cidr -> loc
val cidr_to_idr : cidr -> idr
val cidr_to_string : cidr -> string

module Idr_set : Set.S with type elt = idr
module Idr_map : Map.S with type key = idr

val cpred_loc : cpred -> loc
val ctrm_loc : ctrm -> loc

val cmonad_io : cmonad

val application_depth : int -> idr -> ctrm -> int


(* Specific Symbols *)
val idr_2o_colon : idr
val idr_2o_comma : idr
val idr_2o_arrow : idr
val idr_2o_mapsto : idr
val idr_1o_not : idr
val idr_2o_and : idr
val idr_2o_or : idr
val idr_1o_asterisk : idr
val idr_1b_paren : idr
val idr_2b_dotbracket : idr
val idr_run_toplevel_io : idr
val cidr_is_2o_colon : cidr -> bool
val cidr_is_2o_comma : cidr -> bool
val cidr_is_2o_arrow : cidr -> bool
val cidr_is_2o_mapsto : cidr -> bool
val cidr_is_2o_eq : cidr -> bool
val cidr_is_1o_asterisk : cidr -> bool
val cidr_is_1q_functor : cidr -> bool
val cidr_is_2b_dotbracket : cidr -> bool
