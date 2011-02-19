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

val flatten_application : atyp -> apath * atyp list

val flatten_arrows : atyp -> atyp * atyp list

val fold_apat_vars : (avar -> 'a -> 'a) -> apat -> 'a -> 'a
val fold_apat_typed_vars : (atyp * avar -> 'a -> 'a) -> apat -> 'a -> 'a

val fold_atyp_paths : (apath -> 'a -> 'a) -> atyp -> 'a -> 'a
val fold_apat_paths : (stratum -> apath -> 'a -> 'a) -> apat -> 'a -> 'a
val fold_aval_paths : (stratum -> apath -> 'a -> 'a) -> aval -> 'a -> 'a
val fold_asig_paths : (stratum -> apath -> 'a -> 'a) -> asig -> 'a -> 'a
val fold_amod_paths : (stratum -> apath -> 'a -> 'a) -> amod -> 'a -> 'a
