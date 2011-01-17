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

open Ast_core
open Ast_types
open Diag

let apath_to_avar = function
    | Apath ([], v) -> v
    | p -> errf_at (apath_loc p) "Expecting an unqualified identifier."

let rec result_type = function
    | Atyp_arrow (_, _, at) -> result_type at
    | at -> at

let flatten_application =
    let rec loop args = function
	| Atyp_apply (_, at, arg) -> loop (arg :: args) at
	| Atyp_ref p -> (p, List.rev args)
	| at -> errf_at (atyp_loc at) "Expecting a type constructor." in
    loop []
