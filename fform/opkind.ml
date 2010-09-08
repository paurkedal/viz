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

open Printf
module Fo = Formatter
open FfPervasives

exception Domain_error

type printer = int -> Formatter.t -> unit

type t = {
    ok_name : string;
    ok_arities : int list;
    ok_prec : int;
    ok_print : t * string -> printer list -> printer;
    ok_id : int;
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
let p_rel	= 18
let p_arith n	= 20 + n
let p_apply	= 30
let p_script n	= 32 + n
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

let preinfix_logic = Array.init 7
    (fun n -> {
	ok_name = sprintf "L%d" n;
	ok_arities = [1; 2];
	ok_prec = p_logic n;
	ok_print = print_infix;
	ok_id = make_id ();
    })
let mixfix_quantifier = {
    ok_name = "Q";
    ok_arities = [];
    ok_prec = p_rel;
    ok_print = print_quantifier;
    ok_id = make_id ();
}
let transfix_relation = {
    ok_name = "R";
    ok_arities = [];
    ok_prec = p_rel;
    ok_print = print_unimplemented;
    ok_id = make_id ();
}
let preinfix_arith = Array.init 10
    (fun n -> {
	ok_name = sprintf "A%d" n;
	ok_arities = [1; 2];
	ok_prec = p_arith n;
	ok_print = print_infix;
	ok_id = make_id ();
    })
let suffix_arith = Array.init 10
    (fun n -> {
	ok_name = sprintf "A%dS" n;
	ok_arities = [1];
	ok_prec = p_arith n;
	ok_print = print_suffix;
	ok_id = make_id ();
    })
let prefix_script = Array.init 10
    (fun n -> {
	ok_name = sprintf "S%dP" n;
	ok_arities = [1];
	ok_prec = p_script n;
	ok_print = print_prefix;
	ok_id = make_id ();
    })
let suffix_script = Array.init 10
    (fun n -> {
	ok_name = sprintf "S%dS" n;
	ok_arities = [1];
	ok_prec = p_script n;
	ok_print = print_suffix;
	ok_id = make_id ();
    })
let infix_script = Array.init 10
    (fun n -> {
	ok_name = sprintf "S%dI" n;
	ok_arities = [2];
	ok_prec = p_script n;
	ok_print = print_infix;
	ok_id = make_id ();
    })
let identifier_quote = {
    ok_name = "I";
    ok_arities = [0];
    ok_prec = p_max;
    ok_print = print_unimplemented;
    ok_id = make_id ();
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
	    | ('L', ch) -> preinfix_logic.(int_of_digit ch)
	    | ('S', ch) -> infix_script.(int_of_digit ch)
	    | _ -> raise Domain_error
	    end else
	if n = 3 then
	    begin match String.get s 0, String.get s 1, String.get s 2 with
	    | ('A', ch, 'S') -> suffix_arith.(int_of_digit ch)
	    | ('S', ch, 'I') -> infix_script.(int_of_digit ch)
	    | ('S', ch, 'P') -> prefix_script.(int_of_digit ch)
	    | ('S', ch, 'S') -> suffix_script.(int_of_digit ch)
	    | _ -> raise Domain_error
	    end else
	raise Domain_error
    with Failure _ -> raise Domain_error

let to_string ok = ok.ok_name
