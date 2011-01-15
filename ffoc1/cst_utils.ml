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

open Cst
open Diag

let extract_term_typing = function
    | Trm_apply (_, Trm_apply (_, Trm_ref (_, colon, _), x), y)
	    when colon = i_2o_colon ->
	(x, y)
    | trm -> errf_at (trm_location trm) "Type judgement expected."

let extract_idr_typing expr =
    match extract_term_typing expr with
    | Trm_ref (_, idr, _), y -> (idr, y)
    | x, y -> errf_at (trm_location x) "Identifier expected."

let move_typing (src, dst) =
    match src with
    | Trm_apply (l1, Trm_apply (l2, (Trm_ref (_, op, _) as colon), src'), rsig)
	    when op = i_2o_colon ->
	(src', Trm_apply (l1, Trm_apply (l2, colon, dst), rsig))
    | _ ->
	(src, dst)

let rec move_applications (src, dst) =
    match src with
    | Trm_apply (loc', src', arg) ->
	move_applications (src', Trm_lambda (loc', arg, dst))
    | _ -> (src, dst)

let flatten_tycon_application typ =
    let rec loop args = function
	| Trm_apply (_, con, arg) -> loop (arg :: args) con
	| Trm_ref (_, con, _) -> (con, List.rev args)
	| trm -> errf_at (trm_location trm) "Not a type constructor." in
    loop [] typ
