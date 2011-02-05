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

let extract_ctrm_coercion = function
    | Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (op, _), x), y)
	    when cidr_is_2o_colon op ->
	(x, Some y)
    | x ->
	(x, None)

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

let rec fold_ctrm_args f (trm, accu) =
    match trm with
    | Ctrm_apply (loc, trm', arg) -> fold_ctrm_args f (trm', f arg accu)
    | _ -> (trm, accu)

let rec move_applications (src, dst) =
    match src with
    | Ctrm_apply (loc', src', arg) ->
	move_applications (src', Cpred_at (loc', [arg, dst]))
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

let rec cpred_is_pure = function
    | Cpred_let (_, None, v, x, y) ->
	cpred_is_pure x && cpred_is_pure y
    | Cpred_let (_, Some _, v, x, y) ->
	cpred_is_pure y
    | Cpred_if (_, cond, cq, ccq) ->
	ctrm_is_pure cond && cpred_is_pure cq && cpred_is_pure ccq
    | Cpred_at (_, cases) ->
	List.for_all (fun (_, cq) -> cpred_is_pure cq) cases
    | Cpred_be (_, x) -> ctrm_is_pure x
    | Cpred_do1 _ -> false
    | Cpred_do2 _ -> false
    | Cpred_raise _ -> false
and ctrm_is_pure = function
    | Ctrm_literal _ -> true
    | Ctrm_ref _ -> true
    | Ctrm_label (_, _, x) -> ctrm_is_pure x
    | Ctrm_project _ -> true
    | Ctrm_rel (_, x, rels) ->
	ctrm_is_pure x && List.for_all (fun (_, op, y) -> ctrm_is_pure y) rels
    | Ctrm_apply (_, x, y) -> ctrm_is_pure x && ctrm_is_pure y
    | Ctrm_what (_, Some _, _) -> true
    | Ctrm_what (_, None, x) -> cpred_is_pure x
    | Ctrm_with _ | Ctrm_where _ | Ctrm_quantify _ ->
	raise (Failure "Unreachable.")
