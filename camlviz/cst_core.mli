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

(** Core Functions for the Concrete Syntax Tree *)

open Unicode
open Leaf_types
open Cst_types

val cidr_loc : cidr -> loc
val cidr_to_idr : cidr -> idr
val cidr_to_string : cidr -> string

val cpred_loc : cpred -> loc
val ctrm_loc : ctrm -> loc
val cdef_loc : cdef -> loc

val cmonad_io : cmonad

val application_depth : int -> idr -> ctrm -> int

val ctrm_eq_ref : idr -> ctrm -> bool


(* Specific Symbols *)
val idr_2o_colon : idr
val idr_2o_comma : idr
val idr_1o_semicolon : idr
val idr_2o_semicolon : idr
val idr_2o_vertical_bar : idr
val idr_2o_arrow : idr
val idr_2o_times : idr
val idr_2o_implies : idr
val idr_2o_mapsto : idr
val idr_2o_index : idr
val idr_2o_eq : idr
val idr_2o_coloneq : idr
val idr_2o_A : idr
val idr_2o_E : idr
val idr_1o_not : idr
val idr_2o_and : idr
val idr_2o_or : idr
val idr_1o_asterisk : idr
val idr_1b_paren : idr
val idr_1b_square_bracket : idr
val idr_1b_curly_bracket : idr
val idr_1b_array : idr
val idr_2b_dotbracket : idr
val idr_2b_as : idr
val idr_run_action : idr
val idr_action_throw : idr
val idr_catch : idr
val idr_list_null : idr
val idr_list_push : idr
val idr_set_null : idr
val idr_set_push : idr
val idr_map_null : idr
val idr_map_push : idr
val cidr_is_2o_colon : cidr -> bool
val cidr_is_2o_comma : cidr -> bool
val cidr_is_1o_semicolon : cidr -> bool
val cidr_is_2o_semicolon : cidr -> bool
val cidr_is_2o_arrow : cidr -> bool
val cidr_is_2o_implies : cidr -> bool
val cidr_is_2o_mapsto : cidr -> bool
val cidr_is_2o_index : cidr -> bool
val cidr_is_2o_eq : cidr -> bool
val cidr_is_2o_coloneq : cidr -> bool
val cidr_is_1o_asterisk : cidr -> bool
val cidr_is_1q_functor : cidr -> bool
val cidr_is_2b_dotbracket : cidr -> bool
