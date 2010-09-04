(* Copyright 2010  Petter Urkedal
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
include Input_types

let idr_of_string name = Idr name
let idr_to_string (Idr name) = name
let idr_of_ustring name = Idr (UString.to_utf8 name)
let idr_to_ustring (Idr name) = UString.of_utf8 name
let idr_1o (Idr name) = Idr ("1o" ^ name)
let idr_2o (Idr name) = Idr ("2o" ^ name)

let trm_ref loc name = Trm_ref (loc, idr_of_string name)

let tuple_op = trm_ref Location.dummy "2o,"

let lit_to_string = function
    | Lit_unit -> "unit"
    | Lit_int i -> string_of_int i
    | Lit_float x -> string_of_float x
    | Lit_string s -> "\"" ^ String.escaped (UString.to_utf8 s) ^ "\""

module Fo = Formatter

let print_name fo idr =
    Fo.put fo `Name (idr_to_string idr)

let rec put_infixl fo p_rule p_cur op x y =
    if p_rule < p_cur then Fo.put fo `Operator "(";
    print_inline fo p_rule x;
    Fo.put_op fo op;
    print_inline fo (p_rule + 1) x;
    if p_rule < p_cur then Fo.put fo `Operator ")"

and print_rel_left fo = function
    | Trm_rel_left (_, Idr op, x, y) ->
	print_rel_left fo x;
	Fo.put_op fo op;
	print_inline fo (Opkind.p_rel + 1) y
    | x ->
	print_inline fo (Opkind.p_rel + 1) x

and print_inline fo p = function
    | Trm_ref (_, idr) -> print_name fo idr
    | Trm_literal (_, lit) -> Fo.put fo `Literal (lit_to_string lit)
    | Trm_label (_, Idr label, body) ->
	Fo.put fo `Label (label ^ ":");
	Fo.space fo;
	print_inline fo Opkind.p_apply body
    | Trm_lambda (_, var, body) -> put_infixl fo Opkind.p_cond p "↦" var body
    | Trm_quantify (_, Idr op, var, body) ->
	if p >= Opkind.p_rel then Fo.put fo `Operator "(";
	Fo.put fo `Operator op;
	print_inline fo Opkind.p_rel var;
	Fo.put fo `Operator ".";
	Fo.space fo;
	print_inline fo Opkind.p_rel body;
	if p >= Opkind.p_rel then Fo.put fo `Operator ")"
    | Trm_rel (_, Idr op, x, y) ->
	if p > Opkind.p_rel then Fo.put fo `Operator "(";
	print_rel_left fo x;
	Fo.put_op fo op;
	print_inline fo (Opkind.p_rel + 1) y;
	if p > Opkind.p_rel then Fo.put fo `Operator ")"
    | Trm_apply (_, f, x) ->
	if p > Opkind.p_apply then Fo.put fo `Operator "(";
	print_inline fo Opkind.p_apply f;
	Fo.space fo;
	print_inline fo (Opkind.p_apply + 1) x;
	if p > Opkind.p_apply then Fo.put fo `Operator ")";
    | Trm_where (_, defs) ->
	Fo.put_kw fo "where";
	Fo.enter_indent fo;
	List.iter (print_def fo) defs;
	Fo.leave_indent fo
    | _ ->
	Fo.put fo `Error "(unimplemented)"

and print_predicate ?(default = print_is) fo = function
    | Trm_let (_, var, def, body) ->
	Fo.newline fo;
	Fo.put_kw fo "given";
	print_inline fo Opkind.p_min var;
	Fo.enter_indent fo;
	print_predicate fo def;
	Fo.leave_indent fo;
	print_predicate fo body
    | Trm_if (_, cond, cq, ccq) ->
	Fo.newline fo;
	Fo.put_kw fo "if";
	print_inline fo Opkind.p_min cond;
	Fo.enter_indent fo;
	print_predicate fo cq;
	Fo.leave_indent fo;
	print_predicate ~default:print_else fo ccq
    | Trm_at (_, cases) ->
	List.iter (fun (pat, cq) ->
	    Fo.newline fo;
	    Fo.put_kw fo "at";
	    print_inline fo Opkind.p_min pat;
	    Fo.enter_indent fo;
	    print_predicate fo cq;
	    Fo.leave_indent fo;
	) cases
    | trm -> default fo trm
and print_is fo trm =
    Fo.put_kw fo "is";
    print_inline fo Opkind.p_min trm
and print_else fo trm =
    Fo.newline fo;
    Fo.put_kw fo "else";
    Fo.put_kw fo "is";
    print_inline fo Opkind.p_min trm

and print_def fo def =
    Fo.newline fo;
    match def with
    | Dec_type (_, pat) ->
	Fo.put_kw fo "type";
	print_inline fo Opkind.p_min pat
    | Dec_struct (_, pat, signat) ->
	Fo.put_kw fo "struct";
	print_inline fo (Opkind.p_typing + 1) pat;
	Fo.put_op fo ":";
	print_inline fo (Opkind.p_typing + 1) signat
    | Def_struct (_, pat, body) ->
	Fo.put_kw fo "struct";
	print_inline fo Opkind.p_min pat;
	Fo.put_kw fo "is";
	print_inline fo Opkind.p_min body
    | Dec_sig (_, name) ->
	Fo.put_kw fo "sig";
	print_name fo name
    | Def_sig (_, name, body) ->
	Fo.put_kw fo "sig";
	print_name fo name;
	Fo.put_kw fo "is";
	print_inline fo Opkind.p_min body
    | Dec_val (_, name, typ) ->
	Fo.put_kw fo "val";
	print_name fo name;
	Fo.put_op fo ":";
	print_inline fo Opkind.p_min typ
    | Def_val (_, pat, pred) ->
	Fo.put_kw fo "val";
	print_inline fo Opkind.p_min pat;
	Fo.enter_indent fo;
	print_predicate fo pred;
	Fo.leave_indent fo
    | Def_inj (_, name, typ) ->
	Fo.put_kw fo "inj";
	print_name fo name;
	Fo.put_op fo ":";
	print_inline fo Opkind.p_min typ
    | Dec_lex (_, ok, idrs) ->
	Fo.put_kw fo "lex";
	Fo.put fo `Name (Opkind.to_string ok);
	List.iter (fun (Idr s) -> Fo.space fo; Fo.put fo `Operator s) idrs
    | _ ->
	Fo.put fo `Error "(unimplemented def)"

let print fo =
    print_inline fo Opkind.p_min
