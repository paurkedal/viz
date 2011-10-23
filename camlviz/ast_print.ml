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

open Unicode
open Leaf_types
open Leaf_core
open Ast_types
open Ast_core
open Opkind (* to borrow precedence levels. *)
module Fo = Formatter

let p_let = p_min
let p_arrow = p_arith 1
let p_semi = p_logic 1
let p_eqdef = p_logic 1
let p_mapsto = p_logic 3
let p_passto = p_logic 3
let p_implies = p_logic 3
let p_colon = p_logic 1
let p_qbody = p_logic 0

let print_avar fo v = Fo.put fo `Name (avar_name v)

let print_apath fo (Apath (_, p)) = Fo.put fo `Name (Modpath.to_string p)

let rec print_atyp fo p = function
    | Atyp_ref qn -> print_apath fo qn
    | Atyp_uvar v -> print_avar fo v
    | Atyp_A (_, v, t) ->
	Fo.put fo `Operator "∀";
	print_avar fo v;
	Fo.put fo `Operator ".";
	Fo.space fo;
	print_atyp fo p_qbody t
    | Atyp_E (_, v, t) ->
	Fo.put fo `Operator "∃";
	print_avar fo v;
	Fo.put fo `Operator ".";
	Fo.space fo;
	print_atyp fo p_qbody t
    | Atyp_apply (_, t, u) ->
	if p > p_apply then Fo.put fo `Operator "(";
	print_atyp fo p_apply t;
	Fo.space fo;
	print_atyp fo (p_apply + 1) u;
	if p > p_apply then Fo.put fo `Operator ")"
    | Atyp_arrow (_, t, u) ->
	if p > p_arrow then Fo.put fo `Operator "(";
	print_atyp fo (p_arrow + 1) t;
	Fo.space fo;
	Fo.put fo `Operator "→";
	Fo.space fo;
	print_atyp fo p_arrow u;
	if p > p_arrow then Fo.put fo `Operator ")"

let rec print_apat fo p = function
    | Apat_literal (_, lit) -> Fo.put fo `Literal (lit_to_string lit)
    | Apat_ref qn -> print_apath fo qn
    | Apat_uvar v -> print_avar fo v
    | Apat_apply (_, x, y) ->
	if p > p_apply then Fo.put fo `Operator "(";
	print_apat fo p_apply x;
	Fo.space fo;
	print_apat fo (p_apply + 1) y;
	if p > p_apply then Fo.put fo `Operator ")"
    | Apat_as (_, v, x) ->
	print_avar fo v;
	Fo.put fo `Operator "@(";
	print_apat fo p_min x;
	Fo.put fo `Operator ")"
    | Apat_intype (_, t, x) ->
	if p > p_colon then Fo.put fo `Operator "(";
	print_apat fo (p_colon + 1) x;
	Fo.space fo; Fo.put fo `Operator ":"; Fo.space fo;
	print_atyp fo (p_colon + 1) t;
	if p > p_colon then Fo.put fo `Operator ")"

let rec print_aval fo p = function
    | Aval_literal (_, lit) -> Fo.put fo `Literal (lit_to_string lit)
    | Aval_ref qn -> print_apath fo qn
    | Aval_apply (_, x, y) ->
	if p > p_apply then Fo.put fo `Operator "(";
	print_aval fo p_apply x;
	Fo.space fo;
	print_aval fo (p_apply + 1) y;
	if p > p_apply then Fo.put fo `Operator ")"
    | Aval_array (_, xs) ->
	Fo.put fo `Operator "#[";
	begin match xs with
	| [] -> ()
	| x :: xs ->
	    print_aval fo (p_semi + 1) x;
	    Fo.put fo `Operator ";";
	    List.iter
		begin fun x ->
		    Fo.space fo;
		    print_aval fo (p_semi + 1) x;
		    Fo.put fo `Operator ";"
		end  xs
	end;
	Fo.put fo `Operator "]"
    | Aval_at (_, cases) ->
	if p > p_semi then Fo.put fo `Operator "(";
	let print_case (pat, cond, cq) =
	    print_apat fo (p_mapsto + 1) pat;
	    begin match cond with
	    | None -> ()
	    | Some cond ->
		Fo.space fo;
		Fo.put fo `Operator "[";
		print_aval fo p_min cond;
		Fo.put fo `Operator "]"
	    end;
	    Fo.space fo; Fo.put fo `Operator "↦"; Fo.space fo;
	    print_aval fo (p_mapsto + 1) cq in
	begin match cases with
	| [] -> assert false
	| case :: cases ->
	    print_case case;
	    List.iter
		begin fun case ->
		    Fo.put fo `Operator ";";
		    Fo.space fo;
		    print_case case
		end cases;
	end;
	if p > p_semi then Fo.put fo `Operator ")"
    | Aval_match (loc, x, cases) ->
	if p > p_passto then Fo.put fo `Operator "(";
	print_aval fo (p_passto + 1) x;
	Fo.space fo; Fo.put fo `Operator "@>"; Fo.space fo;
	print_aval fo (p_passto + 1) (Aval_at (loc, cases));
	if p > p_passto then Fo.put fo `Operator ")"
    | Aval_let (_, pat, x, y) ->
	if p > p_let then Fo.put fo `Operator "(";
	Fo.put fo `Keyword "let"; Fo.space fo;
	print_apat fo (p_eqdef + 1) pat;
	Fo.space fo; Fo.put fo `Operator ":="; Fo.space fo;
	print_aval fo (p_eqdef + 1) x;
	Fo.space fo; Fo.put fo `Keyword "in"; Fo.space fo;
	print_aval fo p_let y;
	if p > p_let then Fo.put fo `Operator ")"
    | Aval_letrec (_, bindings, body) ->
	if p > p_let then Fo.put fo `Operator "(";
	Fo.put fo `Keyword "letrec"; Fo.space fo;
	let print_case (_, v, t_opt, x) =
	    print_avar fo v;
	    begin match t_opt with
	    | None -> ()
	    | Some t ->
		Fo.space fo; Fo.put fo `Operator ":"; Fo.space fo;
		print_atyp fo (p_colon + 1) t
	    end;
	    Fo.space fo; Fo.put fo `Operator ":="; Fo.space fo;
	    print_aval fo (p_eqdef + 1) x in
	print_case (List.hd bindings);
	List.iter
	    begin fun case ->
		Fo.space fo; Fo.put fo `Keyword "and"; Fo.space fo;
		print_case case
	    end (List.tl bindings);
	if p > p_let then Fo.put fo `Operator ")"
    | Aval_if (_, cond, cq, ccq) ->
	if p > p_semi then Fo.put fo `Operator "(";
	print_aval fo (p_implies + 1) cond;
	Fo.space fo; Fo.put fo `Operator "⇒"; Fo.space fo;
	print_aval fo p_implies cq;
	Fo.put fo `Operator ";"; Fo.space fo;
	print_aval fo p_semi ccq;
	if p > p_semi then Fo.put fo `Operator ")"
    | Aval_back _ ->
	Fo.put fo `Keyword "backtrack"
    | Aval_seq (_, op, x, y_opt) ->
	if p > p_let then Fo.put fo `Operator "(";
	Fo.put fo `Keyword (idr_to_string op); Fo.space fo;
	print_aval fo p_min x;
	begin match y_opt with
	| None -> ()
	| Some y ->
	    Fo.space fo; Fo.put fo `Keyword "then"; Fo.space fo;
	    print_aval fo p_let y;
	end;
	if p > p_let then Fo.put fo `Operator ")"
    | Aval_raise (_, x) ->
	if p > p_apply then Fo.put fo `Operator "(";
	Fo.put fo `Keyword "raise"; Fo.space fo;
	print_aval fo (p_apply + 1) x;
	if p > p_apply then Fo.put fo `Operator ")"
    | Aval_intype (_, t, x) ->
	Fo.put fo `Operator "(";
	print_atyp fo (p_colon + 1) t;
	Fo.space fo; Fo.put fo `Operator ":"; Fo.space fo;
	print_aval fo (p_colon + 1) x;
	Fo.put fo `Operator ")"

let atyp_to_string atyp =
    let fo = Fo.create () in
    print_atyp fo (p_semi + 1) atyp;
    Fo.contents fo
let apat_to_string apat =
    let fo = Fo.create () in
    print_apat fo (p_semi + 1) apat;
    Fo.contents fo
let aval_to_string aval =
    let fo = Fo.create () in
    print_aval fo (p_semi + 1) aval;
    Fo.contents fo
