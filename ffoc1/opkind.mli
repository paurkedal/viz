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

(** Classification and Information about Operators *)

open Unicode
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

(** The maximum ok_id assignment plus one. *)
val maxp_id : int

(** Returns the record describing the operator kind of the given name. *)
val of_string : string -> t

(** Returns the name of an operator kind. *)
val to_string : t -> string

(* The operator kinds.  These correspond to the similarly named terminals of the
 * grammar of type [Cst.ctrm]. *)
val    mixfix_quantifier : t
val  preinfix_logic      : t array
val  transfix_relation   : t
val  preinfix_arith      : t array
val    suffix_arith      : t array
val     infix_script     : t array
val    prefix_script     : t array
val    suffix_script     : t array
val    suffix_project    : t
val circumfix_lbracket   : t
val circumfix_rbracket   : t
val postcircumfix_lbracket : t
val identifier_quote     : t

(* Precedence levels. *)
val p_min : int
val p_typing : int
val p_comma : int
val p_cond : int
val p_logic : int -> int
val p_rel : int
val p_arith : int -> int
val p_apply : int
val p_script : int -> int
val p_project : int
val p_max : int

