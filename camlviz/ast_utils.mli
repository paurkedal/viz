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

(** Auxiliary Functions for the Abstract Syntax Tree *)

open Leaf_types
open Ast_types
open Ast_core

val atyp_to_string : atyp -> string
val apat_to_string : apat -> string
val aval_to_string : aval -> string
val asig_to_string : asig -> string
val amod_to_string : amod -> string

val apath_to_avar : apath -> avar

val result_type : atyp -> atyp
val fold_arg_types : (atyp -> 'a -> 'a) -> atyp -> 'a -> 'a
val arity : atyp -> int

val atyp_unapply : atyp -> apath * atyp list

val atyp_apply : apath -> atyp list -> atyp

type pocket =
    | No_pocket
    | Local_pocket of avar
    | World_pocket

val atyp_is_const : atyp -> bool

val atyp_action_pocket : atyp -> pocket
val unwrap_atyp_action : atyp -> pocket * atyp

val flatten_arrows : atyp -> atyp * atyp list
val flatten_arrows_for_c : atyp -> bool * atyp * atyp list

val fold_apat_vars : (avar -> 'a -> 'a) -> apat -> 'a -> 'a
val fold_apat_typed_vars : (atyp * avar -> 'a -> 'a) -> apat -> 'a -> 'a

val fold_atyp_paths : (apath -> 'a -> 'a) -> atyp -> 'a -> 'a
val fold_apat_paths : (stratum -> apath -> 'a -> 'a) -> apat -> 'a -> 'a
val fold_aval_paths : (stratum -> apath -> 'a -> 'a) -> aval -> 'a -> 'a
val fold_asig_paths : (stratum -> apath -> 'a -> 'a) -> asig -> 'a -> 'a
val fold_amod_paths : ?module_name: string ->
		      (stratum -> apath -> 'a -> 'a) -> amod -> 'a -> 'a

val fold_amod_cabi_open : (string -> 'a -> 'a) -> amod -> 'a -> 'a
val fold_adef_cabi_open : (string -> 'a -> 'a) -> adef -> 'a -> 'a

val interpret_use : aval -> ause
