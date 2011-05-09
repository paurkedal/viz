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

open FfPervasives
open Unicode
open Leaf_types
open Cst_types
open Cst_core
open Diag

let fold_cpred_sub fp ft fd = function
    | Cpred_let (_, _, x, p, q) -> ft x *> fp p *> fp q
    | Cpred_if (_, x, p, q) -> ft x *> fp q *> fp q
    | Cpred_back _ -> ident
    | Cpred_at (_, bx) -> List.fold (fun (x, p) -> ft x *> fp p) bx
    | Cpred_be (_, x) -> ft x
    | Cpred_assert (_, x, p) -> ft x *> fp p
    | Cpred_trace (_, x, p) -> ft x *> fp p
    | Cpred_raise (_, x) -> ft x
    | Cpred_do1 (_, _, x) -> ft x
    | Cpred_do2 (_, _, x, p) -> ft x *> fp p
    | Cpred_upon (_, x, p, q) -> ft x *> fp p *> fp q
let fold_ctrm_sub fp ft fd = function
    | Ctrm_ref _ | Ctrm_literal _ -> ident
    | Ctrm_label (_, _, x) -> ft x
    | Ctrm_quantify (_, _, x, y) -> ft x *> ft y
    | Ctrm_rel (_, x, rs) -> ft x *> List.fold (fun (_, _, x) -> ft x) rs
    | Ctrm_apply (_, x, y) -> ft x *> ft y
    | Ctrm_project (_, _, x) -> ft x
    | Ctrm_array (_, xs) -> List.fold ft xs
    | Ctrm_what (_, _, p) -> fp p
    | Ctrm_where (_, ds) -> List.fold fd ds
    | Ctrm_with (_, xo, ds) -> Option.fold ft xo *> List.fold fd ds

let for_all_cpred_sub fp ft fd = function
    | Cpred_let (_, _, x, p, q) -> ft x && fp p && fp q
    | Cpred_if (_, x, p, q) -> ft x && fp q && fp q
    | Cpred_back _ -> true
    | Cpred_at (_, bx) -> List.for_all (fun (x, p) -> ft x && fp p) bx
    | Cpred_be (_, x) -> ft x
    | Cpred_assert (_, x, p) -> ft x && fp p
    | Cpred_trace (_, x, p) -> ft x && fp p
    | Cpred_raise (_, x) -> ft x
    | Cpred_do1 (_, _, x) -> ft x
    | Cpred_do2 (_, _, x, p) -> ft x && fp p
    | Cpred_upon (_, x, p, q) -> ft x && fp p && fp q
let for_all_ctrm_sub fp ft fd = function
    | Ctrm_ref _ | Ctrm_literal _ -> true
    | Ctrm_label (_, _, x) -> ft x
    | Ctrm_quantify (_, _, x, y) -> ft x && ft y
    | Ctrm_rel (_, x, rs) -> ft x && List.for_all (fun (_, _, x) -> ft x) rs
    | Ctrm_apply (_, x, y) -> ft x && ft y
    | Ctrm_project (_, _, x) -> ft x
    | Ctrm_array (_, xs) -> List.for_all ft xs
    | Ctrm_what (_, _, p) -> fp p
    | Ctrm_where (_, ds) -> List.for_all fd ds
    | Ctrm_with (_, xo, ds) -> Option.for_all ft xo && List.for_all fd ds

let for_some_cpred_sub fp ft fd p =
    not (for_all_cpred_sub (fp *> not) (ft *> not) (fd *> not) p)

let for_some_ctrm_sub fp ft fd x =
    not (for_all_ctrm_sub (fp *> not) (ft *> not) (fd *> not) x)

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

let extract_term_cname_opt = function
    | Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (op, _), x),
	    Ctrm_literal (_, Lit_string y))
	    when cidr_is_2o_coloneq op ->
	(x, Some (UString.to_utf8 y))
    | x -> (x, None)

let move_typing (src, dst) =
    match src with
    | Ctrm_apply (l1, Ctrm_apply (l2, (Ctrm_ref (op, _) as colon), src'), rsig)
	    when cidr_is_2o_colon op ->
	(src', Ctrm_apply (l1, Ctrm_apply (l2, colon, dst), rsig))
    | _ ->
	(src, dst)

let is_injname cidr = cidr_is_2o_comma cidr

let count_formal_args ctrm =
    let rec loop n = function
	| Ctrm_apply (loc, f, _) -> loop (n + 1) f
	| Ctrm_ref (cidr, Ih_inj) -> 0
	| Ctrm_ref (cidr, Ih_univ) -> n
	| Ctrm_ref (cidr, Ih_none) when not (is_injname cidr) -> n
	| _ -> 0 in
    loop 0 ctrm

let rec fold_ctrm_args f (trm, accu) =
    match trm with
    | Ctrm_apply (loc, trm', arg) ->
	fold_ctrm_args f (trm', f arg accu)
    | Ctrm_rel (loc, cx, [_, cf, cy]) ->
	fold_ctrm_args f (Ctrm_ref (cf, Ih_none), f cy (f cx accu))
    | _ -> (trm, accu)

let fold_formal_args f (trm, accu) =
    if count_formal_args trm = 0 then (trm, accu) else
    fold_ctrm_args f (trm, accu)

let rec fold_functor_args f (trm, accu) =
    match trm with
    | Ctrm_apply (loc, Ctrm_apply (_, op, trm'), arg)
	    when ctrm_eq_ref idr_2b_dotparen op ->
	fold_functor_args f (trm', f arg accu)
    | _ ->
	    (trm, accu)

let rec is_formal = function
    | Ctrm_ref (cidr, Ih_inj) -> false
    | Ctrm_ref (cidr, Ih_univ) -> true
    | Ctrm_ref (cidr, Ih_none) -> not (is_injname cidr)
    | Ctrm_label (_, _, x) -> is_formal x
    | Ctrm_quantify _ -> assert false
    | Ctrm_rel (_, _, [_]) -> true
    | Ctrm_rel _ -> false
    | Ctrm_apply (_, x, _) -> is_formal x
    | _ -> false

let rec formal_idr = function (* only valid if the above succeeds *)
    | Ctrm_ref (_, Ih_inj) -> assert false
    | Ctrm_ref (Cidr (_, idr), _) -> idr
    | Ctrm_label (_, _, x)
    | Ctrm_apply (_, x, _) -> formal_idr x
    | Ctrm_rel (_, _, [(_, Cidr (_, idr), _)]) -> idr
    | _ -> assert false

let rec cpred_uses_shadowed idr p =
    for_some_cpred_sub (cpred_uses_shadowed idr) (ctrm_uses_shadowed idr)
		       (fun _ -> false) p
and ctrm_uses_shadowed idr = function
    | Ctrm_ref (Cidr (_, idr'), Ih_inj) when idr' = idr -> true
    | x ->
	for_some_ctrm_sub (cpred_uses_shadowed idr) (ctrm_uses_shadowed idr)
			  (fun _ -> false) x

let collect_pattern_vars x =
    let rec coll fpos = function
	| Ctrm_ref (Cidr (_, idr), Ih_inj) -> ident
	| Ctrm_ref (Cidr (_, idr), _) ->
	    if fpos then ident else fun idrs -> idr :: idrs
	| Ctrm_literal _ -> ident
	| Ctrm_label (_, _, x) -> coll fpos x
	| Ctrm_quantify _ -> assert false (* unimplemented *)
	| Ctrm_rel (_, x, rys) ->
	    List.fold (fun (_, _, y) -> coll false y) rys *< coll false x
	| Ctrm_apply (_, f, x) ->
	    coll true f *< coll false x
	| Ctrm_project (_, _, x) -> coll fpos x
	| Ctrm_array (_, xs) -> List.fold (coll false) xs
	| Ctrm_what _ | Ctrm_where _ | Ctrm_with _ ->
	    assert false (* If reachable, write out a proper error message. *)
    in coll false x []

let rec move_applications (src, dst) =
    match src with
    | Ctrm_apply (loc', src', arg) ->
	move_applications (src', Cpred_at (loc', [arg, dst]))
    | Ctrm_rel (loc', x, [(_, op, y)]) ->
	(Ctrm_ref (op, Ih_none),
	 Cpred_at (loc', [x, Cpred_at (loc', [y, dst])]))
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

let rec fold_on_semicolon f = function
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (op, _), x), y)
	    when cidr_is_2o_semicolon op ->
	f x *> fold_on_semicolon f y
    | x -> f x

let rec fold_on_comma f = function
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (op, _), x), y)
	    when cidr_is_2o_comma op ->
	fold_on_comma f x *> f y
    | x -> f x

let rec cpred_is_pure = function
    | Cpred_let (_, None, v, x, y) ->
	cpred_is_pure x && cpred_is_pure y
    | Cpred_let (_, Some _, v, x, y) ->
	cpred_is_pure y
    | Cpred_if (_, cond, cq, ccq) ->
	ctrm_is_pure cond && cpred_is_pure cq && cpred_is_pure ccq
    | Cpred_back _ -> true
    | Cpred_at (_, cases) ->
	List.for_all (fun (_, cq) -> cpred_is_pure cq) cases
    | Cpred_be (_, x) -> ctrm_is_pure x
    | Cpred_do1 _ -> false
    | Cpred_do2 _ -> false
    | Cpred_upon _ -> false
    | Cpred_assert (_, x, y) -> cpred_is_pure y
    | Cpred_trace (_, x, y) -> cpred_is_pure y
    | Cpred_raise _ -> false
and ctrm_is_pure = function
    | Ctrm_literal _ -> true
    | Ctrm_ref _ -> true
    | Ctrm_label (_, _, x) -> ctrm_is_pure x
    | Ctrm_project _ -> true
    | Ctrm_array (_, xs) -> List.for_all ctrm_is_pure xs
    | Ctrm_rel (_, x, rels) ->
	ctrm_is_pure x && List.for_all (fun (_, op, y) -> ctrm_is_pure y) rels
    | Ctrm_apply (_, x, y) -> ctrm_is_pure x && ctrm_is_pure y
    | Ctrm_what (_, Some _, _) -> true
    | Ctrm_what (_, None, x) -> cpred_is_pure x
    | Ctrm_with _ | Ctrm_where _ | Ctrm_quantify _ ->
	assert false (* unreachable *)

let ctrm_is_exception_type t =
    match flatten_arrow t with
    | Ctrm_ref (Cidr (loc, Idr "exception"), _), _ -> true
    | _ -> false
