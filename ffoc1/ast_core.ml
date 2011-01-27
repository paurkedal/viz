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

open Ast_types
open Cst_types
open Leaf_types
open FfPervasives

let avar_idr (Avar (_, idr)) = idr
let avar_name (Avar (_, Idr s)) = s
let avar_loc (Avar (loc, _)) = loc

let apath_loc = function
    | Apath ([], v) -> avar_loc v
    | Apath (avs, v) ->
	Location.span [avar_loc (List.last avs); avar_loc v]

let atyp_loc = function
    | Atyp_ref p -> apath_loc p
    | Atyp_uvar v -> avar_loc v
    | Atyp_apply (loc, _, _) -> loc
    | Atyp_arrow (loc, _, _) -> loc

let rec aval_loc = function
    | Aval_literal (loc, _) -> loc
    | Aval_ref p -> apath_loc p
    | Aval_apply (loc, _, _) -> loc
    | Aval_at (loc, _) -> loc
    | Aval_let (loc, _, _) -> loc
    | Aval_if (loc, _, _, _) -> loc
    | Aval_match (loc, _, _) -> loc
    | Aval_raise (loc, _) -> loc

let rec apat_loc = function
    | Apat_ref p -> apath_loc p
    | Apat_uvar x -> avar_loc x
    | Apat_apply (loc, _, _) -> loc

let rec asig_loc = function
    | Asig_ref p -> apath_loc p
    | Asig_decs (loc, bs) -> loc
    | Asig_product (loc, _, _, _) -> loc
    | Asig_with_type (loc, _, _, _) -> loc
    | Asig_with_struct (loc, _, _, _) -> loc
and adec_loc = function
    | Adec_include (loc, _) -> loc
    | Adec_open (loc, _) -> loc
    | Adec_in (loc, _, _) -> loc
    | Adec_sig (loc, _, _) -> loc
    | Adec_types bs ->
	let (lloc, _, _, _) = List.hd bs in
	let (uloc, _, _, _) = List.last bs in
	Location.span [lloc; uloc]
    | Adec_val (loc, _, _) -> loc

and amod_loc = function
    | Amod_ref p -> apath_loc p
    | Amod_defs (loc, _) -> loc
    | Amod_apply (loc, _, _) -> loc
    | Amod_lambda (loc, _, _, _) -> loc
    | Amod_coercion (loc, _, _) -> loc
and adef_loc = function
    | Adef_include (loc, _) -> loc
    | Adef_open (loc, _) -> loc
    | Adef_in (loc, _, _) -> loc
    | Adef_sig (loc, _, _) -> loc
    | Adef_types bs ->
	let (lloc, _, _, _) = List.hd bs in
	let (uloc, _, _, _) = List.last bs in
	Location.span [lloc; uloc]
    | Adef_val (loc, _, _) -> loc
    | Adef_vals bs ->
	let (lloc, _, _) = List.hd bs in
	let (uloc, _, _) = List.last bs in
	Location.span [lloc; uloc]
