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

open Printf
module Fo = Formatter
open FfPervasives
open Cst_types
open Leaf_types

exception Domain_error
exception Invalid_definition of string

type printer = int -> Formatter.t -> unit

type lexkind = Lex_regular | Lex_intro | Lex_continued

type t = {
    ok_name : string;
    ok_arities : int list;
    ok_prec : int;
    ok_print : t * string -> printer list -> printer;
    ok_id : int;
    ok_lexkind : lexkind;
    ok_create : idr * idr list -> Grammar.token;
}

let next_id = ref 0
let make_id () = let id = !next_id in next_id := id + 1; id


(* Operator Precedences *)

let p_min	=  0
let p_typing	=  1
let p_comma	=  2
let p_cond	=  3
let p_quant	=  4
let p_logic n	= 10 + n
let p_rel	= 19
let p_arith n	= 20 + n
let p_apply	= 30
let p_script n	= 32 + n
let p_project	= 35
let p_max	= 36


(* Print Functions *)

let print_quantifier (ok, opname) fs p fo =
    match fs with
    | [fx; fy] ->
	if ok.ok_prec <= p then Fo.put fo `Operator "(";
	fx p_rel fo;
	Fo.put fo `Operator ".";
	Fo.space fo;
	fy p fo;
	if ok.ok_prec <= p then Fo.put fo `Operator ")"
    | _ -> raise (Failure "Wrong number of sub-printers for print_quantifier.")

let print_prefix (ok, opname) fs p fo =
    match fs with
    | [fx] ->
	if ok.ok_prec <= p then Fo.put fo `Operator "(";
	Fo.put fo `Operator opname;
	Fo.space fo;
	fx p fo;
	if ok.ok_prec <= p then Fo.put fo `Operator ")";
    | _ -> raise (Failure "Wrong number of sub-printers for print_prefix")

let print_suffix (ok, opname) fs p fo =
    match fs with
    | [fx] ->
	if ok.ok_prec <= p then Fo.put fo `Operator "(";
	fx p fo;
	Fo.space fo;
	Fo.put fo `Operator opname;
	if ok.ok_prec <= p then Fo.put fo `Operator ")";
    | _ -> raise (Failure "Wrong number of sub-printers for print_prefix")

let print_infix (ok, opname) fs p fo =
    match fs with
    | [fx] ->
	(* Arithmetic infix operators can also be used as prefix operators. *)
	print_prefix (ok, opname) fs p fo
    | [fx; fy] ->
	let dp = ok.ok_prec mod 2 in
	if ok.ok_prec <= p then Fo.put fo `Operator "(";
	fx (p + dp) fo;
	Fo.space fo;
	Fo.put fo `Operator opname;
	Fo.space fo;
	fy (p + 1 - dp) fo;
	if ok.ok_prec <= p then Fo.put fo `Operator ")"
    | _ -> raise (Failure "Wrong number of sub-printers for print_infix")

let print_unimplemented (ok, opname) fs p fo =
    raise (Failure "No printer for this operator kind.")


(* Operator Kind Definitions *)

let name_raw = function
    | (op, []) | (_, [op]) -> op
    | _ -> raise (Invalid_definition "Expected a single identifier.")
let name_0o = function
    | (op, []) -> Leaf_core.idr_0o op
    | (_, [o0]) -> o0
    | _ -> raise (Invalid_definition "Expected a single identifier.")
let name_1o = function
    | (op, []) -> Leaf_core.idr_1o op
    | (_, [o1]) -> o1
    | _ -> raise (Invalid_definition "Expected a single identifier.")
let name_2o = function
    | (op, []) -> Leaf_core.idr_2o op
    | (_, [o2]) -> o2
    | _ -> raise (Invalid_definition "Expected a single identifier.")
let name_1o2o = function
    | (op, []) -> (Leaf_core.idr_1o op, Leaf_core.idr_2o op)
    | (_, [o1; o2]) -> (o1, o2)
    | (_, [Idr o12]) when String.starts_with "12'" o12 ->
	let s = String.after 3 o12 in (Idr ("1'" ^ s), Idr ("2'" ^ s))
    | _ -> raise (Invalid_definition "Expected two identifiers.")

let preinfix_logic = Array.init 9
    (fun n -> {
	ok_name = sprintf "L%d" n;
	ok_arities = [1; 2];
	ok_prec = p_logic n;
	ok_print = print_infix;
	ok_id = make_id ();
	ok_lexkind = Lex_regular;
	ok_create =
	    begin match n with
	    | 0 -> fun spec -> Grammar.LOGIC0 (name_1o2o spec)
	    | 1 -> fun spec -> Grammar.LOGIC1 (name_1o2o spec)
	    | 2 -> fun spec -> Grammar.LOGIC2 (name_1o2o spec)
	    | 3 -> fun spec -> Grammar.LOGIC3 (name_1o2o spec)
	    | 4 -> fun spec -> Grammar.LOGIC4 (name_1o2o spec)
	    | 5 -> fun spec -> Grammar.LOGIC5 (name_1o2o spec)
	    | 6 -> fun spec -> Grammar.LOGIC6 (name_1o2o spec)
	    | 7 -> fun spec -> Grammar.LOGIC7 (name_1o2o spec)
	    | 8 -> fun spec -> Grammar.LOGIC8 (name_1o2o spec)
	    | _ -> raise (Failure "Unreachable.")
	    end;
    })
let mixfix_quantifier = {
    ok_name = "Q";
    ok_arities = [];
    ok_prec = p_rel;
    ok_print = print_quantifier;
    ok_id = make_id ();
    ok_lexkind = Lex_regular;
    ok_create = (fun spec -> Grammar.QUANTIFIER (name_2o spec));
}
let transfix_relation = {
    ok_name = "R";
    ok_arities = [];
    ok_prec = p_rel;
    ok_print = print_unimplemented;
    ok_id = make_id ();
    ok_lexkind = Lex_regular;
    ok_create = (fun spec -> Grammar.RELATION (name_2o spec));
}
let preinfix_arith = Array.init 10
    (fun n -> {
	ok_name = sprintf "A%d" n;
	ok_arities = [1; 2];
	ok_prec = p_arith n;
	ok_print = print_infix;
	ok_id = make_id ();
	ok_lexkind = Lex_regular;
	ok_create =
	    begin match n with
	    | 0 -> fun spec -> Grammar.ARITH0 (name_1o2o spec)
	    | 1 -> fun spec -> Grammar.ARITH1 (name_1o2o spec)
	    | 2 -> fun spec -> Grammar.ARITH2 (name_1o2o spec)
	    | 3 -> fun spec -> Grammar.ARITH3 (name_1o2o spec)
	    | 4 -> fun spec -> Grammar.ARITH4 (name_1o2o spec)
	    | 5 -> fun spec -> Grammar.ARITH5 (name_1o2o spec)
	    | 6 -> fun spec -> Grammar.ARITH6 (name_1o2o spec)
	    | 7 -> fun spec -> Grammar.ARITH7 (name_1o2o spec)
	    | 8 -> fun spec -> Grammar.ARITH8 (name_1o2o spec)
	    | 9 -> fun spec -> Grammar.ARITH9 (name_1o2o spec)
	    | _ -> raise (Failure "Unreachable.")
	    end;
    })
let suffix_arith = Array.init 10
    (fun n -> {
	ok_name = sprintf "A%dS" n;
	ok_arities = [1];
	ok_prec = p_arith n;
	ok_print = print_suffix;
	ok_id = make_id ();
	ok_lexkind = Lex_regular;
	ok_create =
	    begin match n with
	    | 0 -> fun spec -> Grammar.ARITH0_S (name_1o spec)
	    | 1 -> fun spec -> Grammar.ARITH1_S (name_1o spec)
	    | 2 -> fun spec -> Grammar.ARITH2_S (name_1o spec)
	    | 3 -> fun spec -> Grammar.ARITH3_S (name_1o spec)
	    | 4 -> fun spec -> Grammar.ARITH4_S (name_1o spec)
	    | 5 -> fun spec -> Grammar.ARITH5_S (name_1o spec)
	    | 6 -> fun spec -> Grammar.ARITH6_S (name_1o spec)
	    | 7 -> fun spec -> Grammar.ARITH7_S (name_1o spec)
	    | 8 -> fun spec -> Grammar.ARITH8_S (name_1o spec)
	    | 9 -> fun spec -> Grammar.ARITH9_S (name_1o spec)
	    | _ -> raise (Failure "Unreachable.")
	    end;
    })
let prefix_script = Array.init 3
    (fun n -> {
	ok_name = sprintf "S%dP" n;
	ok_arities = [1];
	ok_prec = p_script n;
	ok_print = print_prefix;
	ok_id = make_id ();
	ok_lexkind = Lex_regular;
	ok_create =
	    begin match n with
	    | 0 -> fun spec -> Grammar.SCRIPT0_P (name_1o spec)
	    | 1 -> fun spec -> Grammar.SCRIPT1_P (name_1o spec)
	    | 2 -> fun spec -> Grammar.SCRIPT2_P (name_1o spec)
	    | _ -> raise (Failure "Unreachable.")
	    end;
    })
let suffix_script = Array.init 3
    (fun n -> {
	ok_name = sprintf "S%dS" n;
	ok_arities = [1];
	ok_prec = p_script n;
	ok_print = print_suffix;
	ok_id = make_id ();
	ok_lexkind = Lex_regular;
	ok_create =
	    begin match n with
	    | 0 -> fun spec -> Grammar.SCRIPT0_S (name_1o spec)
	    | 1 -> fun spec -> Grammar.SCRIPT1_S (name_1o spec)
	    | 2 -> fun spec -> Grammar.SCRIPT2_S (name_1o spec)
	    | _ -> raise (Failure "Unreachable.")
	    end;
    })
let infix_script = Array.init 3
    (fun n -> {
	ok_name = sprintf "S%dI" n;
	ok_arities = [2];
	ok_prec = p_script n;
	ok_print = print_infix;
	ok_id = make_id ();
	ok_lexkind = Lex_regular;
	ok_create =
	    begin match n with
	    | 0 -> fun spec -> Grammar.SCRIPT0_I (name_2o spec)
	    | 1 -> fun spec -> Grammar.SCRIPT1_I (name_2o spec)
	    | 2 -> fun spec -> Grammar.SCRIPT2_I (name_2o spec)
	    | _ -> raise (Failure "Unreachable.")
	    end;
    })
let suffix_project = {
    ok_name = "PS";
    ok_arities = [1];
    ok_prec = p_project;
    ok_print = print_unimplemented;
    ok_id = make_id ();
    ok_lexkind = Lex_regular;
    ok_create = (fun spec -> Grammar.PROJECT (name_raw spec));
}
let circumfix_lbracket = {
    ok_name = "BL";
    ok_arities = [1];
    ok_prec = p_max;
    ok_print = print_unimplemented;
    ok_id = make_id ();
    ok_lexkind = Lex_regular;
    ok_create = (fun spec -> Grammar.LBRACKET (name_raw spec));
}
let circumfix_rbracket = {
    ok_name = "BR";
    ok_arities = [1];
    ok_prec = p_max;
    ok_print = print_unimplemented;
    ok_id = make_id ();
    ok_lexkind = Lex_regular;
    ok_create = (fun spec -> Grammar.RBRACKET (name_raw spec));
}
let postcircumfix_lbracket = {
    ok_name = "PL";
    ok_arities = [2];
    ok_prec = p_project;
    ok_print = print_unimplemented;
    ok_id = make_id ();
    ok_lexkind = Lex_regular;
    ok_create = (fun spec -> Grammar.PROJECT_LBRACKET (name_raw spec));
}
let identifier_quote = {
    ok_name = "I";
    ok_arities = [0];
    ok_prec = p_max;
    ok_print = print_unimplemented;
    ok_id = make_id ();
    ok_lexkind = Lex_regular;
    ok_create = (fun spec -> Grammar.IDENTIFIER (name_0o spec));
}

let maxp_id = !next_id

let of_string s =
    let n = String.length s in
    try
	if n = 0 then raise Domain_error else
	if n = 1 then
	    begin match String.get s 0 with
	    | 'I' -> identifier_quote
	    | 'Q' -> mixfix_quantifier
	    | 'R' -> transfix_relation
	    | _ -> raise Domain_error
	    end else
	if n = 2 then
	    begin match String.get s 0, String.get s 1 with
	    | ('A', ch) -> preinfix_arith.(int_of_digit ch)
	    | ('B', 'L') -> circumfix_lbracket
	    | ('B', 'R') -> circumfix_rbracket
	    | ('L', ch) -> preinfix_logic.(int_of_digit ch)
	    | ('S', ch) -> infix_script.(int_of_digit ch)
	    | _ -> raise Domain_error
	    end else
	if n = 3 then
	    begin match String.get s 0, String.get s 1, String.get s 2 with
	    | ('A', ch, 'S') -> suffix_arith.(int_of_digit ch)
	    | ('S', '2', 'L') -> postcircumfix_lbracket
	    | ('S', ch, 'I') -> infix_script.(int_of_digit ch)
	    | ('S', ch, 'P') -> prefix_script.(int_of_digit ch)
	    | ('S', ch, 'S') -> suffix_script.(int_of_digit ch)
	    | _ -> raise Domain_error
	    end else
	raise Domain_error
    with Failure _ -> raise Domain_error

let to_string ok = ok.ok_name
