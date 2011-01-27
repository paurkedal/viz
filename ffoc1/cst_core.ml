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

open Cst_types
open Leaf_types
open Leaf_core

let cidr_loc (Cidr (loc, _)) = loc
let cidr_to_idr (Cidr (_, idr)) = idr
let cidr_to_string (Cidr (_, Idr s)) = s

let idr_2o_colon	= idr_2o_c ":"
let idr_2o_arrow	= idr_2o_c "→"
let idr_2o_mapsto	= idr_2o_c "↦"
let idr_2o_comma	= idr_2o_c ","
let idr_2o_eq		= idr_2o_c "="
let idr_1o_not		= idr_2o_c "¬"
let idr_2o_and		= idr_2o_c "∧"
let idr_2o_or		= idr_2o_c "∨"
let idr_1q_functor	= idr_1q_c "Fun"
let idr_2b_dotbracket	= idr_2b_c ".[" "]"
let idr_1o_asterisk	= idr_1o_c "*"
let cidr_is_2o_colon	(Cidr (_, idr)) = idr = idr_2o_colon
let cidr_is_2o_arrow	(Cidr (_, idr)) = idr = idr_2o_arrow
let cidr_is_2o_mapsto	(Cidr (_, idr)) = idr = idr_2o_mapsto
let cidr_is_2o_comma	(Cidr (_, idr)) = idr = idr_2o_comma
let cidr_is_2o_eq	(Cidr (_, idr)) = idr = idr_2o_eq
let cidr_is_1q_functor	(Cidr (_, idr)) = idr = idr_1q_functor
let cidr_is_1o_asterisk	(Cidr (_, idr)) = idr = idr_1o_asterisk
let cidr_is_2b_dotbracket (Cidr (_, idr)) = idr = idr_2b_dotbracket

module Idr = struct
    type t = idr
    let compare (Idr x) (Idr y) = compare x y
end
module Idr_set = Set.Make (Idr)
module Idr_map = Map.Make (Idr)

let ctrm_loc = function
    | Ctrm_ref (Cidr (loc, _), _) | Ctrm_literal (loc, _) | Ctrm_label (loc, _, _)
    | Ctrm_quantify (loc, _, _, _)
    | Ctrm_let (loc, _, _, _)
    | Ctrm_rel (loc, _, _) | Ctrm_apply (loc, _, _)
    | Ctrm_project (loc, _, _)
    | Ctrm_raise (loc, _) | Ctrm_if (loc, _, _, _) | Ctrm_at (loc, _)
    | Ctrm_where (loc, _) | Ctrm_with (loc, _, _) ->
	loc

let application_depth i f x =
    let rec loop n xs = function
	| Ctrm_apply (_, Ctrm_ref (Cidr (_, f'), _), x') when f' = f ->
	    loop (n + 1) [] (List.nth (x' :: xs) i)
	| Ctrm_apply (_, f', x') ->
	    loop n (x' :: xs) f'
	| _ -> n in
    loop 0 [] x
