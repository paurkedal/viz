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

open FfPervasives
open Leaf_types
open Cst_types
open Cst_core

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
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	Ctrm_project (loc, l, x), accu
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
    | Cdef_open (loc, x), accu ->
	let x, accu = rw.rw_ctrm rw stra (x, accu) in
	Cdef_open (loc, x), accu
    | Cdef_type (loc, t), accu ->
	let t, accu = rw.rw_ctrm rw `Type (t, accu)  in
	Cdef_type (loc, t), accu
    | Cdef_in (loc, x, y), accu ->
	let x, accu = rw.rw_ctrm rw `Structure (x, accu) in
	let y, accu = rw.rw_ctrm rw stra (y, accu) in
	Cdef_in (loc, x, y), accu
    | Cdec_sig (loc, name), accu -> Cdec_sig (loc, name), accu
    | Cdef_sig (loc, name, x), accu ->
	let x, accu = rw.rw_ctrm rw `Signature (x, accu) in
	Cdef_sig (loc, name, x), accu
    | Cdec_val (loc, t), accu ->
	let t, accu = rw.rw_ctrm rw `Type (t, accu) in
	Cdec_val (loc, t), accu
    | Cdef_val (loc, export, cm, x, p), accu ->
	let x, accu = rw.rw_ctrm rw `Value (x, accu) in
	let p, accu = rw.rw_cpred rw `Value (p, accu) in
	Cdef_val (loc, export, cm, x, p), accu
    | Cdef_inj (loc, t), accu ->
	let t, accu = rw.rw_ctrm rw `Type (t, accu) in
	Cdef_inj (loc, t), accu
    | Cdef_lex _, accu as d -> d

let default_rewrite_cpred = subterm_rewrite_cpred

let rec rewrite_coll_comma push rw inner = function
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (Cidr (_, op), _), xs), x), accu
	    when op = idr_2o_comma ->
	let x, accu = rw.rw_ctrm rw `Value (x, accu) in
	let inner = Ctrm_apply (loc, Ctrm_apply (loc, push, x), inner) in
	rewrite_coll_comma push rw inner (xs, accu)
    | x, accu ->
	let loc = ctrm_loc x in
	let x, accu = rw.rw_ctrm rw `Value (x, accu) in
	Ctrm_apply (loc, Ctrm_apply (loc, push, x), inner), accu

let rewrite_coll_semicolon (null, push) rw = function
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (Cidr (_, op), _), x), xr), accu
	    when op = idr_2o_semicolon ->
	let xr, accu = rw.rw_ctrm rw `Value (xr, accu) in
	rewrite_coll_comma push rw xr (x, accu)
    | d -> rewrite_coll_comma push rw null d

let rewrite_coll_comprehension (null, push) rw = function
    | Ctrm_literal (loc, Lit_unit), accu -> null, accu
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (Cidr (_, op), _), x), y), accu
	    when op = idr_2o_vertical_bar ->
	assert false (* unimplemented *)
    | d -> rewrite_coll_semicolon (null, push) rw d

let default_rewrite_ctrm_value rw = function
    | Ctrm_apply (loc, Ctrm_ref (Cidr (op_loc, op), _), x), accu
	    when op = idr_1b_square_bracket ->
	let null = Ctrm_ref (Cidr (op_loc, idr_list_null), Ih_inj) in
	let push = Ctrm_ref (Cidr (op_loc, idr_list_push), Ih_inj) in
	rewrite_coll_comprehension (null, push) rw (x, accu)
    | d -> subterm_rewrite_ctrm rw `Value d

let default_rewrite_ctrm rw stra = function
    | Ctrm_apply (loc, Ctrm_ref (Cidr (_, op), _), x), accu
	    when op = idr_1b_paren ->
	rw.rw_ctrm rw stra (x, accu)
    | d ->
	match stra with
	| `Value -> default_rewrite_ctrm_value rw d
	| _ -> subterm_rewrite_ctrm rw stra d

let default_rewrite_cdef = subterm_rewrite_cdef

let default_rewriter = {
    rw_cpred = default_rewrite_cpred;
    rw_ctrm = default_rewrite_ctrm;
    rw_cdef = default_rewrite_cdef;
}
