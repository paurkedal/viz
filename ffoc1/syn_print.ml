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

open Unicode
open Cst_types
open Cst_core
open Leaf_types
open Leaf_core
module Fo = Formatter

let print_name fo (Cidr (_, idr)) =
    Fo.put fo `Name (idr_to_string idr)

let print_hinted_name fo idr idrhint =
    Fo.enter fo `Name;
    Fo.put_string fo (idr_to_string idr);
    begin match idrhint with
    | Ih_none -> ()
    | Ih_univ -> Fo.put_string fo "_"
    | Ih_inj -> Fo.put_string fo "`"
    end;
    Fo.leave fo `Name

let rec put_infixl fo p_rule p_cur op x y =
    if p_rule < p_cur then Fo.put fo `Operator "(";
    print_inline fo p_rule x;
    Fo.put_op fo op;
    print_inline fo (p_rule + 1) y;
    if p_rule < p_cur then Fo.put fo `Operator ")"

and print_inline fo p = function
    | Ctrm_ref (Cidr (_, idr), idrhint) ->
	print_hinted_name fo idr idrhint
    | Ctrm_literal (_, lit) -> Fo.put fo `Literal (lit_to_string lit)
    | Ctrm_label (_, Cidr (_, Idr label), body) ->
	Fo.put fo `Label (label ^ ":");
	Fo.space fo;
	print_inline fo Opkind.p_apply body
    | Ctrm_quantify (_, Cidr (_, Idr op), var, body) ->
	if p >= Opkind.p_rel then Fo.put fo `Operator "(";
	Fo.put fo `Operator op;
	let op_u = UString.of_string op in
	if UChar.is_idrchr (UString.get op_u (UString.length op_u - 1)) then
	    Fo.space fo;
	print_inline fo Opkind.p_rel var;
	Fo.put fo `Operator ".";
	Fo.space fo;
	print_inline fo Opkind.p_rel body;
	if p >= Opkind.p_rel then Fo.put fo `Operator ")"
    | Ctrm_rel (_, x, rels) ->
	if p > Opkind.p_rel then Fo.put fo `Operator "(";
	print_inline fo (Opkind.p_rel + 1) x;
	List.iter begin fun (_, Cidr (_, opname), y) ->
	    Fo.put_op fo (idr_2o_symbol opname);
	    print_inline fo (Opkind.p_rel + 1) y;
	end rels;
	if p > Opkind.p_rel then Fo.put fo `Operator ")"
    | Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (op, _), x), y)
	    when cidr_is_2o_colon op ->
	put_infixl fo Opkind.p_typing p ":" x y
    | Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (op, _), x), y)
	    when cidr_is_2o_arrow op ->
	put_infixl fo (Opkind.p_logic 5) p "→" x y
    | Ctrm_apply (_, f, x) ->
	if p > Opkind.p_apply then Fo.put fo `Operator "(";
	print_inline fo Opkind.p_apply f;
	Fo.space fo;
	print_inline fo (Opkind.p_apply + 1) x;
	if p > Opkind.p_apply then Fo.put fo `Operator ")";
    | Ctrm_project (_, Cidr (_, Idr field), m) ->
	print_inline fo Opkind.p_project m;
	Fo.put fo `Name ("." ^ field)
    | Ctrm_with (_, base, defs) ->
	Option.iter (print_inline fo p) base;
	Fo.put_kw fo "with";
	print_defs fo defs
    | Ctrm_where (_, defs) ->
	Fo.put_kw fo "where";
	print_defs fo defs
    | _ ->
	Fo.put fo `Error "(unimplemented)"

and print_predicate fo = function
    | Cpred_let (_, cm_opt, var, cdef, body) ->
	Fo.newline fo;
	begin match cm_opt with
	| None -> Fo.put_kw fo "let";
	| Some cm -> Fo.put_kw fo ("let!" ^ cm)
	end;
	print_inline fo Opkind.p_min var;
	Fo.enter_indent fo;
	print_predicate fo cdef;
	Fo.leave_indent fo;
	print_predicate fo body
    | Cpred_if (_, cond, cq, ccq) ->
	Fo.newline fo;
	Fo.put_kw fo "if";
	print_inline fo Opkind.p_min cond;
	Fo.enter_indent fo;
	print_predicate fo cq;
	Fo.leave_indent fo;
	print_predicate fo ccq
    | Cpred_at (_, cases) ->
	List.iter (fun (pat, cq) ->
	    Fo.newline fo;
	    Fo.put_kw fo "at";
	    print_inline fo Opkind.p_min pat;
	    Fo.enter_indent fo;
	    print_predicate fo cq;
	    Fo.leave_indent fo;
	) cases
    | Cpred_be (_, ctrm) ->
	Fo.newline fo;
	Fo.put_kw fo "be";
	print_inline fo Opkind.p_min ctrm
    | Cpred_do1 (_, cm, cx) ->
	Fo.newline fo;
	Fo.put_kw fo "do";
	print_inline fo Opkind.p_min cx
    | Cpred_do2 (_, cm, cx, cpred) ->
	Fo.newline fo;
	Fo.put_kw fo "do";
	print_inline fo Opkind.p_min cx;
	print_predicate fo cpred
    | Cpred_raise (_, cx) ->
	Fo.newline fo;
	Fo.put_kw fo "raise";
	print_inline fo Opkind.p_min cx

and print_def fo cdef =
    Fo.newline fo;
    match cdef with
    | Cdef_include (_, path) ->
	Fo.put_kw fo "include";
	print_inline fo Opkind.p_min path
    | Cdef_open (_, path) ->
	Fo.put_kw fo "open";
	print_inline fo Opkind.p_min path
    | Cdef_type (_, eqn) ->
	Fo.put_kw fo "type";
	print_inline fo Opkind.p_min eqn
    | Cdef_in (_, pat, body) ->
	Fo.put_kw fo "in";
	Fo.enter_indent fo;
	print_inline fo Opkind.p_min pat;
	Fo.leave_indent fo;
	begin match body with
	| Ctrm_with (_, None, defs) -> print_defs fo defs
	| Ctrm_where (_, defs) -> print_defs fo defs
	| _ -> Fo.put_kw fo "include"; print_inline fo Opkind.p_min body
	end
    | Cdec_sig (_, name) ->
	Fo.put_kw fo "sig";
	print_name fo name
    | Cdef_sig (_, name, body) ->
	Fo.put_kw fo "sig";
	print_name fo name;
	begin match body with
	| Ctrm_with (_, None, defs) -> print_defs fo defs
	| _ -> Fo.put_kw fo "include"; print_inline fo Opkind.p_min body
	end
    | Cdec_val (_, typing) ->
	Fo.put_kw fo "val";
	print_inline fo Opkind.p_min typing
    | Cdef_val (_, export, cm_opt, pat, pred) ->
	let kw = if export then "val" else "let" in
	let kw = Option.fold (fun cm kw -> kw ^ cm) cm_opt kw in
	Fo.put_kw fo kw;
	print_inline fo Opkind.p_min pat;
	Fo.enter_indent fo;
	print_predicate fo pred;
	Fo.leave_indent fo
    | Cdef_inj (_, typing) ->
	Fo.put_kw fo "inj";
	print_inline fo Opkind.p_min typing
    | Cdef_lex (_, okname, idrs) ->
	Fo.put_kw fo "lex";
	Fo.put fo `Name okname;
	let put_lexdef (Cidr (_, Idr s), names) =
	    Fo.space fo; Fo.put fo `Operator s;
	    match names with
	    | Cidr (_, Idr name) :: names ->
		Fo.put fo `Keyword "(";
		Fo.put fo `Name name;
		let put_name (Cidr (_, Idr name)) =
		    Fo.space fo;
		    Fo.put fo `Name name in
		List.iter put_name names;
		Fo.put fo `Keyword ")"
	    | [] -> () in
	List.iter put_lexdef idrs
and print_defs fo defs =
    Fo.enter_indent fo;
    List.iter (print_def fo) defs;
    Fo.leave_indent fo

let print fo =
    print_inline fo Opkind.p_min

let ctrm_to_string ctrm =
    let fo = Formatter.create () in
    print fo ctrm;
    Formatter.contents fo

let cdef_to_string cdef =
    let fo = Formatter.create () in
    print_def fo cdef;
    Formatter.contents fo
