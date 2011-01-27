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

open Cst_types
open Cst_core
open Diag

let extract_term_typing = function
    | Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (op, _), x), y)
	    when cidr_is_2o_colon op ->
	(x, y)
    | ctrm -> errf_at (ctrm_loc ctrm) "Type judgement expected."

let extract_cidr_typing expr =
    match extract_term_typing expr with
    | Ctrm_ref (cidr, _), y -> (cidr, y)
    | x, y -> errf_at (ctrm_loc x) "Identifier expected."

let move_typing (src, dst) =
    match src with
    | Ctrm_apply (l1, Ctrm_apply (l2, (Ctrm_ref (op, _) as colon), src'), rsig)
	    when cidr_is_2o_colon op ->
	(src', Ctrm_apply (l1, Ctrm_apply (l2, colon, dst), rsig))
    | _ ->
	(src, dst)

let rec move_applications (src, dst) =
    match src with
    | Ctrm_apply (loc', src', arg) ->
	move_applications (src', Ctrm_at (loc', [arg, dst]))
    | _ -> (src, dst)

let flatten_tycon_application typ =
    let rec loop args = function
	| Ctrm_apply (_, con, arg) -> loop (arg :: args) con
	| Ctrm_ref (con, _) -> (con, List.rev args)
	| ctrm -> errf_at (ctrm_loc ctrm) "Not a type constructor." in
    loop [] typ

let rec flatten_arrow typ =
    let rec loop pts = function
	| Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (op, _), pt), rt)
		when cidr_is_2o_arrow op ->
	    loop (pt :: pts) rt
	| rt -> (rt, List.rev pts) in
    loop [] typ
