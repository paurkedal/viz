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
open Leaf_types
open Cst_types
open Cst_core
open Diag

type 'a rewriter = {
    rw_cpred : 'a rewriter -> stratum -> cpred * 'a -> cpred * 'a;
    rw_ctrm  : 'a rewriter -> stratum -> ctrm  * 'a -> ctrm  * 'a;
    rw_cdef  : 'a rewriter -> stratum -> cdef  * 'a -> cdef  * 'a;
}

let rec subterm_rewrite_cpred rw stra = function
    | Cpred_let (loc, cm, x, p, q), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	let p, accu = rw.rw_cpred rw stra (p, accu) in
	let q, accu = rw.rw_cpred rw stra (q, accu) in
	Cpred_let (loc, cm, x, p, q), accu
    | Cpred_if (loc, cond, cq, ccq), accu ->
	let cond, accu = rw.rw_ctrm rw stra (cond, accu) in
	let cq, accu = rw.rw_cpred rw stra (cq, accu) in
	let ccq, accu = rw.rw_cpred rw stra (ccq, accu) in
	Cpred_if (loc, cond, cq, ccq), accu
    | Cpred_back loc, accu ->
	Cpred_back loc, accu
    | Cpred_at (loc, cases), accu ->
	let recase ((x, pred), accu) =
	    let x, accu = rw.rw_ctrm rw stra (x, accu) in
	    let pred, accu = rw.rw_cpred rw stra (pred, accu) in
	    ((x, pred), accu) in
	let cases, accu = List.map_fold recase (cases, accu) in
	Cpred_at (loc, cases), accu
    | Cpred_be (loc, x), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	Cpred_be (loc, x), accu
    | Cpred_assert (loc, x, y), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	let y, accu = rw.rw_cpred rw stra (y, accu) in
	Cpred_assert (loc, x, y), accu
    | Cpred_raise (loc, x), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	Cpred_raise (loc, x), accu
    | Cpred_do1 (loc, cm, x), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	Cpred_do1 (loc, cm, x), accu
    | Cpred_do2 (loc, cm, x, y), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	let y, accu = rw.rw_cpred rw stra (y, accu) in
	Cpred_do2 (loc, cm, x, y), accu
    | Cpred_upon (loc, p, x, y), accu ->
	let p, accu = rw.rw_ctrm rw stra (p, accu) in
	let x, accu = rw.rw_cpred rw stra (x, accu) in
	let y, accu = rw.rw_cpred rw stra (y, accu) in
	Cpred_upon (loc, p, x, y), accu
and subterm_rewrite_ctrm rw stra = function
    | Ctrm_ref _, _ | Ctrm_literal _, _ as d -> d
    | Ctrm_label (loc, l, x), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	Ctrm_label (loc, l, x), accu
    | Ctrm_quantify (loc, v, x, y), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	let y, accu = rw.rw_ctrm rw stra (y, accu) in
	Ctrm_quantify (loc, v, x, y), accu
    | Ctrm_rel (loc, x, xs), accu ->
	let rwrel ((loc, name, x), accu) =
	    let x, accu = rw.rw_ctrm rw stra (x, accu) in
	    ((loc, name, x), accu) in
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	let xs, accu = List.map_fold rwrel (xs, accu) in
	Ctrm_rel (loc, x, xs), accu
    | Ctrm_apply (loc, x, y), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	let y, accu = rw.rw_ctrm rw stra (y, accu) in
	Ctrm_apply (loc, x, y), accu
    | Ctrm_project (loc, l, x), accu ->
	let x, accu = rw.rw_ctrm rw `Structure (x, accu) in
	Ctrm_project (loc, l, x), accu
    | Ctrm_array (loc, xs), accu ->
	let xs, accu = List.map_fold (rw.rw_ctrm rw stra) (xs, accu) in
	Ctrm_array (loc, xs), accu
    | Ctrm_what (loc, cm, p), accu ->
	let p, accu = rw.rw_cpred rw stra (p, accu) in
	Ctrm_what (loc, cm, p), accu
    | Ctrm_where (loc, defs), accu ->
	let defs, accu =
	    List.map_fold (rw.rw_cdef rw `Structure) (defs, accu) in
	Ctrm_where (loc, defs), accu
    | Ctrm_with (loc, mo, defs), accu ->
	let mo, accu = Option.map_fold (rw.rw_ctrm rw `Structure) (mo, accu) in
	let defs, accu =
	    List.map_fold (rw.rw_cdef rw `Signature) (defs, accu) in
	Ctrm_with (loc, mo, defs), accu
and subterm_rewrite_cdef rw stra = function
    | Cdef_include (loc, x), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	Cdef_include (loc, x), accu
    | Cdef_open (loc, abi, x), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	Cdef_open (loc, abi, x), accu
    | Cdef_use (loc, x), accu as d -> d
    | Cdef_type (loc, abi, t, defs), accu ->
	let t, accu = rw.rw_ctrm rw `Type (t, accu)  in
	let defs, accu = List.map_fold (rw.rw_cdef rw stra) (defs, accu) in
	Cdef_type (loc, abi, t, defs), accu
    | Cdef_in (loc, x, y), accu ->
	let x, accu = rw.rw_ctrm rw `Structure (x, accu) in
	let y, accu = rw.rw_ctrm rw stra (y, accu) in
	Cdef_in (loc, x, y), accu
    | Cdec_sig (loc, name), accu -> Cdec_sig (loc, name), accu
    | Cdef_sig (loc, name, x), accu ->
	let x, accu = rw.rw_ctrm rw `Signature (x, accu) in
	Cdef_sig (loc, name, x), accu
    | Cdef_val (loc, vi, t), accu ->
	let t, accu = rw.rw_ctrm rw `Type (t, accu) in
	Cdef_val (loc, vi, t), accu
    | Cdef_let (loc, cm, x, p), accu ->
	let x, accu = rw.rw_ctrm rw `Value (x, accu) in
	let p, accu = rw.rw_cpred rw `Value (p, accu) in
	Cdef_let (loc, cm, x, p), accu
    | Cdef_inj (loc, abi, t), accu ->
	let t, accu = rw.rw_ctrm rw `Type (t, accu) in
	Cdef_inj (loc, abi, t), accu
    | Cdef_lex _, _ | Cdef_lexalias _, _ as d -> d

let default_rewrite_cpred = subterm_rewrite_cpred

(* Alt 1: [x1, ..., xn] and [x1, ..., xn; xr]. *)
let rec rewrite_classicext_comma push rw inner = function
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (Cidr (_, op), _), xs), x), accu
	    when op = idr_2o_comma ->
	let x, accu = rw.rw_ctrm rw `Value (x, accu) in
	let inner = Ctrm_apply (loc, Ctrm_apply (loc, push, x), inner) in
	rewrite_classicext_comma push rw inner (xs, accu)
    | x, accu ->
	let loc = ctrm_loc x in
	let x, accu = rw.rw_ctrm rw `Value (x, accu) in
	Ctrm_apply (loc, Ctrm_apply (loc, push, x), inner), accu
let rewrite_classicext_semicolon (null, push) rw = function
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (Cidr (_, op), _), x), xr), accu
	    when op = idr_2o_semicolon ->
	let xr, accu = rw.rw_ctrm rw `Value (xr, accu) in
	rewrite_classicext_comma push rw xr (x, accu)
    | d -> rewrite_classicext_comma push rw null d

(* Alt 2: [x1; ... xn;] and [x1; ...; xn; xr] *)
let rewrite_primext_push push rw = function
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (mapsto, _), x), y), accu
	    when cidr_is_2o_mapsto mapsto ->
	let x, accu = rw.rw_ctrm rw `Value (x, accu) in
	let y, accu = rw.rw_ctrm rw `Value (y, accu) in
	Ctrm_apply (loc, Ctrm_apply (loc, push, x), y), accu
    | x, accu ->
	let x, accu = rw.rw_ctrm rw `Value (x, accu) in
	Ctrm_apply (ctrm_loc x, push, x), accu
let rec rewrite_primext_semicolon (null, push) rw = function
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (semi, _), x), xr), accu
	    when cidr_is_2o_semicolon semi ->
	let xp, accu = rewrite_primext_push push rw (x, accu) in
	let xr, accu = rewrite_primext_semicolon (null, push) rw (xr, accu) in
	Ctrm_apply (loc, xp, xr), accu
    | Ctrm_apply (loc, Ctrm_ref (semi, _), x), accu
	    when cidr_is_1o_semicolon semi ->
	let xp, accu = rewrite_primext_push push rw (x, accu) in
	Ctrm_apply (loc, xp, null), accu
    | xr, accu -> rw.rw_ctrm rw `Value (xr, accu)
let rewrite_coll_comprehension (null, push) rw = function
    | Ctrm_literal (loc, Lit_unit), accu -> null, accu
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (Cidr (_, op), _), x), y), accu
	    when op = idr_2o_vertical_bar ->
	assert false (* unimplemented *)
    | Ctrm_apply (_, Ctrm_ref (semi, _), _), _ as d
	    when cidr_is_1o_semicolon semi ->
	rewrite_primext_semicolon (null, push) rw d
    | Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (semi, _), _), _), _ as d
	    when cidr_is_2o_semicolon semi ->
	rewrite_primext_semicolon (null, push) rw d
    | d ->
	rewrite_classicext_semicolon (null, push) rw d

let rec rewrite_comma_into_list rw = function
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (Cidr (_, op), _), xs), x),
	    ys, accu when op = idr_2o_comma ->
	let (y, accu) = rw.rw_ctrm rw `Value (x, accu) in
	rewrite_comma_into_list rw (xs, y :: ys, accu)
    | x, ys, accu ->
	let (y, accu) = rw.rw_ctrm rw `Value (x, accu) in
	(y :: ys, accu)

let rewrite_mapsto rw (z, accu) =
    let rwcase = function
	| Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (mapsto, _), x), y)
		when cidr_is_2o_mapsto mapsto ->
	    fun (cases, accu) ->
	    let x, accu = rw.rw_ctrm rw `Value (x, accu) in
	    let y, accu = rw.rw_ctrm rw `Value (y, accu) in
	    ((x, Cpred_be (loc, y)) :: cases, accu)
	| x -> errf_at (ctrm_loc x) "Expecting a mapping." in
    let cases, accu
	= Cst_utils.fold_on_semicolon rwcase z ([], accu) in
    let loc = ctrm_loc z in
    (Ctrm_what (loc, None, Cpred_at (loc, List.rev cases)), accu)

let is_mapsto = function
    | Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (mapsto, _), _), _)
	when cidr_is_2o_mapsto mapsto -> true
    | _ -> false
let is_maplike = function
    | Ctrm_apply (_, Ctrm_ref (semi, _), x)
	when cidr_is_1o_semicolon semi -> is_mapsto x
    | Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (semi, _), x), _)
	when cidr_is_2o_semicolon semi -> is_mapsto x
    | _ -> false
let default_rewrite_ctrm_value rw = function
    | Ctrm_apply (loc, Ctrm_ref (Cidr (op_loc, op), _), x), accu
	    when op = idr_1b_square_bracket ->
	let null = Ctrm_ref (Cidr (op_loc, idr_list_null), Ih_inj) in
	let push = Ctrm_ref (Cidr (op_loc, idr_list_push), Ih_inj) in
	rewrite_coll_comprehension (null, push) rw (x, accu)
    | Ctrm_apply (loc, Ctrm_ref (Cidr (op_loc, op), _), x), accu
	    when op = idr_1b_curly_bracket ->
	let idr_null, idr_push =
	    if is_maplike x then (idr_map_null, idr_map_push)
			    else (idr_set_null, idr_set_push) in
	let null = Ctrm_ref (Cidr (op_loc, idr_null), Ih_inj) in
	let push = Ctrm_ref (Cidr (op_loc, idr_push), Ih_inj) in
	rewrite_coll_comprehension (null, push) rw (x, accu)
    | Ctrm_apply (loc, Ctrm_ref (Cidr (op_loc, op), _), x), accu
	    when op = idr_1b_array ->
	let (ys, accu) = rewrite_comma_into_list rw (x, [], accu) in
	(Ctrm_array (loc, ys), accu)
    | Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (mapsto, _), _), _) as z, accu
	    when cidr_is_2o_mapsto mapsto ->
	rewrite_mapsto rw (z, accu)
    | Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (semi, _), x), _) as z, accu
	    when cidr_is_2o_semicolon semi ->
	begin match x with
	| Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (mapsto, _), _), _)
		when cidr_is_2o_mapsto mapsto ->
	    rewrite_mapsto rw (z, accu)
	| _ -> subterm_rewrite_ctrm rw `Value (z, accu)
	end
    | d -> subterm_rewrite_ctrm rw `Value d

let rewrite_structure_name = function
    | "array" -> "array_"
    | "char" -> "char_"
    | "list" -> "list_"
    | "string" -> "string_"
    | name -> name

let default_rewrite_ctrm_structure rw = function
    | Ctrm_ref (Cidr (loc, Idr name), hint), accu ->
	let name = rewrite_structure_name name in
	Ctrm_ref (Cidr (loc, Idr name), hint), accu
    | Ctrm_project (locp, Cidr (locn, Idr name), x), accu ->
	let (x, accu) = rw.rw_ctrm rw `Structure (x, accu) in
	let name = rewrite_structure_name name in
	Ctrm_project (locp, Cidr (locn, Idr name), x), accu
    | d -> subterm_rewrite_ctrm rw `Structure d

let default_rewrite_ctrm rw stra = function
    | Ctrm_apply (loc, Ctrm_ref (Cidr (_, op), _), x), accu
	    when op = idr_1b_paren ->
	rw.rw_ctrm rw stra (x, accu)
    | d ->
	match stra with
	| `Value -> default_rewrite_ctrm_value rw d
	| `Structure -> default_rewrite_ctrm_structure rw d
	| _ -> subterm_rewrite_ctrm rw stra d

let default_rewrite_cdef = subterm_rewrite_cdef

let default_rewriter = {
    rw_cpred = default_rewrite_cpred;
    rw_ctrm = default_rewrite_ctrm;
    rw_cdef = default_rewrite_cdef;
}
