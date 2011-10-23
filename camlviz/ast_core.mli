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

(** Core Functions for the Abstract Syntax Tree *)

open Ast_types
open Cst_types
open Leaf_types

val avar_idr : avar -> idr
val avar_name : avar -> string

val str_to_lid : string -> string
val str_to_uid : string -> string
val idr_to_lid : idr -> string
val idr_to_uid : idr -> string
val avar_to_lid : avar -> string
val avar_to_uid : avar -> string

val avar_loc : avar -> loc
val apath_loc : apath -> loc
val atyp_loc : atyp -> loc
val aval_loc : aval -> loc
val apat_loc : apat -> loc
val asig_loc : asig -> loc
val adec_loc : adec -> loc
val amod_loc : amod -> loc
val adef_loc : adef -> loc

val apath_to_idr : apath -> idr
val apath_eq_idr : idr -> apath -> bool
val apath_eq_string : string -> apath -> bool
val apath_of_avar : avar -> apath

val apat_uvar_any : loc -> apat
val apat_uvar_of_idr : loc -> idr -> apat
val aval_ref_of_idr : loc -> idr -> aval

val fresh_avar_at : ?prefix: string -> loc -> avar

val aval_string : loc -> string -> aval

val aval_apply1i : loc -> idr -> aval -> aval
val aval_apply2i : loc -> idr -> aval -> aval -> aval

val aval_internal_error : loc -> string -> aval

val atyp_map :
    ?on_apath : (apath -> apath) ->
    ?on_avar : (avar -> avar) ->
    ?on_atyp : (atyp -> atyp) ->
    atyp -> atyp

val aval_map_subaval : (aval -> aval) -> aval -> aval
val amod_map_subaval : (aval -> aval) -> amod -> amod
val adef_map_subaval : (aval -> aval) -> adef -> adef
val amod_map_subamod : (amod -> amod) -> amod -> amod
val adef_map_subamod : (amod -> amod) -> adef -> adef

val atyp_compare : atyp -> atyp -> int
