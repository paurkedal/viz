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
include Cst_types

let starts_with p s =
    let np = String.length p in
    np <= String.length s && p = String.sub s 0 np

let idr_of_string name = Idr name
let idr_to_string (Idr name) = name
let idr_of_ustring name = Idr (UString.to_utf8 name)
let idr_to_ustring (Idr name) = UString.of_utf8 name
let idr_1o_c name = Idr ("1'" ^ name)
let idr_1o (Idr name) = idr_1o_c name
let idr_1o_symbol (Idr name) =
    if starts_with "1'" name then String.sub name 2 (String.length name - 2)
    else raise (Failure ("Expected a unary operator identifier: " ^ name))
let idr_2o_c name = Idr ("2'" ^ name)
let idr_2o (Idr name) = idr_2o_c name
let idr_2o_symbol (Idr name) =
    if starts_with "2'" name then String.sub name 2 (String.length name - 2)
    else raise (Failure ("Expected a binary operator identifier: " ^ name))
let idr_1b_c lname rname = Idr ("1'" ^ lname ^ "'" ^ rname)
let idr_1b (Idr lname) (Idr rname) = idr_1b_c lname rname

let cidr_location (Cidr (loc, _)) = loc
let cidr_to_idr (Cidr (_, idr)) = idr
let cidr_to_string (Cidr (_, Idr s)) = s

let idr_2o_colon	= idr_2o_c ":"
let idr_2o_arrow	= idr_2o_c "→"
let idr_2o_comma	= idr_2o_c ","
let idr_2o_eq		= idr_2o_c "="
let cidr_is_2o_colon	(Cidr (_, idr)) = idr = idr_2o_colon
let cidr_is_2o_arrow	(Cidr (_, idr)) = idr = idr_2o_arrow
let cidr_is_2o_comma	(Cidr (_, idr)) = idr = idr_2o_comma
let cidr_is_2o_eq	(Cidr (_, idr)) = idr = idr_2o_eq

let lit_to_string = function
    | Lit_unit -> "unit"
    | Lit_bool x -> if x then "true" else "false"
    | Lit_int i -> string_of_int i
    | Lit_float x -> string_of_float x
    | Lit_string s -> "\"" ^ String.escaped (UString.to_utf8 s) ^ "\""

module Idr = struct
    type t = idr
    let compare (Idr x) (Idr y) = compare x y
end
module Idr_set = Set.Make (Idr)
module Idr_map = Map.Make (Idr)

let trm_location = function
    | Trm_ref (Cidr (loc, _), _) | Trm_literal (loc, _) | Trm_label (loc, _, _)
    | Trm_lambda (loc, _, _) | Trm_quantify (loc, _, _, _)
    | Trm_let (loc, _, _, _)
    | Trm_rel (loc, _, _) | Trm_apply (loc, _, _)
    | Trm_project (loc, _, _)
    | Trm_raise (loc, _) | Trm_if (loc, _, _, _) | Trm_at (loc, _)
    | Trm_where (loc, _) | Trm_with (loc, _, _) ->
	loc

let application_depth i f x =
    let rec loop n xs = function
	| Trm_apply (_, Trm_ref (Cidr (_, f'), _), x') when f' = f ->
	    loop (n + 1) [] (List.nth (x' :: xs) i)
	| Trm_apply (_, f', x') ->
	    loop n (x' :: xs) f'
	| _ -> n in
    loop 0 [] x

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
    | Trm_ref (Cidr (_, idr), idrhint) ->
	print_hinted_name fo idr idrhint
    | Trm_literal (_, lit) -> Fo.put fo `Literal (lit_to_string lit)
    | Trm_label (_, Cidr (_, Idr label), body) ->
	Fo.put fo `Label (label ^ ":");
	Fo.space fo;
	print_inline fo Opkind.p_apply body
    | Trm_lambda (_, var, body) -> put_infixl fo Opkind.p_cond p "↦" var body
    | Trm_quantify (_, Cidr (_, Idr op), var, body) ->
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
    | Trm_rel (_, x, rels) ->
	if p > Opkind.p_rel then Fo.put fo `Operator "(";
	print_inline fo (Opkind.p_rel + 1) x;
	List.iter begin fun (_, Cidr (_, opname), y) ->
	    Fo.put_op fo (idr_2o_symbol opname);
	    print_inline fo (Opkind.p_rel + 1) y;
	end rels;
	if p > Opkind.p_rel then Fo.put fo `Operator ")"
    | Trm_apply (_, Trm_apply (_, Trm_ref (op, _), x), y)
	    when cidr_is_2o_colon op ->
	put_infixl fo Opkind.p_typing p ":" x y
    | Trm_apply (_, Trm_apply (_, Trm_ref (op, _), x), y)
	    when cidr_is_2o_arrow op ->
	put_infixl fo (Opkind.p_logic 5) p "→" x y
    | Trm_apply (_, f, x) ->
	if p > Opkind.p_apply then Fo.put fo `Operator "(";
	print_inline fo Opkind.p_apply f;
	Fo.space fo;
	print_inline fo (Opkind.p_apply + 1) x;
	if p > Opkind.p_apply then Fo.put fo `Operator ")";
    | Trm_project (_, Cidr (_, Idr field), m) ->
	print_inline fo Opkind.p_project m;
	Fo.put fo `Name ("." ^ field)
    | Trm_with (_, base, defs) ->
	Option.iter (print_inline fo p) base;
	Fo.put_kw fo "with";
	print_defs fo defs
    | Trm_where (_, defs) ->
	Fo.put_kw fo "where";
	print_defs fo defs
    | _ ->
	Fo.put fo `Error "(unimplemented)"

and print_predicate ?(default = print_is) fo = function
    | Trm_let (_, var, def, body) ->
	Fo.newline fo;
	Fo.put_kw fo "let";
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
    | trm ->
	Fo.newline fo;
	default fo trm
and print_is fo trm =
    Fo.put_kw fo "be";
    print_inline fo Opkind.p_min trm
and print_else fo trm =
    Fo.newline fo;
    Fo.put_kw fo "else";
    Fo.put_kw fo "be";
    print_inline fo Opkind.p_min trm

and print_def fo def =
    Fo.newline fo;
    match def with
    | Sct_include (_, path) ->
	Fo.put_kw fo "include";
	print_inline fo Opkind.p_min path
    | Sct_open (_, path) ->
	Fo.put_kw fo "open";
	print_inline fo Opkind.p_min path
    | Sct_type (_, eqn) ->
	Fo.put_kw fo "type";
	print_inline fo Opkind.p_min eqn
    | Sct_in (_, pat, body) ->
	Fo.put_kw fo "in";
	Fo.enter_indent fo;
	print_inline fo Opkind.p_min pat;
	Fo.leave_indent fo;
	begin match body with
	| Trm_with (_, None, defs) -> print_defs fo defs
	| Trm_where (_, defs) -> print_defs fo defs
	| _ -> Fo.put_kw fo "include"; print_inline fo Opkind.p_min body
	end
    | Dec_sig (_, name) ->
	Fo.put_kw fo "sig";
	print_name fo name
    | Def_sig (_, name, body) ->
	Fo.put_kw fo "sig";
	print_name fo name;
	begin match body with
	| Trm_with (_, None, defs) -> print_defs fo defs
	| _ -> Fo.put_kw fo "include"; print_inline fo Opkind.p_min body
	end
    | Dec_val (_, typing) ->
	Fo.put_kw fo "val";
	print_inline fo Opkind.p_min typing
    | Def_val (_, pat, pred) ->
	Fo.put_kw fo "val";
	print_inline fo Opkind.p_min pat;
	Fo.enter_indent fo;
	print_predicate fo pred;
	Fo.leave_indent fo
    | Dec_inj (_, typing) ->
	Fo.put_kw fo "inj";
	print_inline fo Opkind.p_min typing
    | Dec_lex (_, ok, idrs) ->
	Fo.put_kw fo "lex";
	Fo.put fo `Name (Opkind.to_string ok);
	List.iter (fun (Cidr (_, Idr s)) -> Fo.space fo; Fo.put fo `Operator s)
		  idrs
and print_defs fo defs =
    Fo.enter_indent fo;
    List.iter (print_def fo) defs;
    Fo.leave_indent fo

let print fo =
    print_inline fo Opkind.p_min

let trm_to_string trm =
    let fo = Formatter.create () in
    print fo trm;
    Formatter.contents fo
