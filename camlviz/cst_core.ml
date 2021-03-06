(* Copyright (C) 2010--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Cst_types
open Leaf_types
open Leaf_core

let cidr_loc (Cidr (loc, _)) = loc
let cidr_to_idr (Cidr (_, idr)) = idr
let cidr_to_string (Cidr (_, Idr s)) = s

let ctrm_eq_ref idr = function
  | Ctrm_ref (Cidr (_, idr'), _) -> idr = idr'
  | _ -> false

let idr_kw_be           = Idr "be"
let idr_kw_fail         = Idr "fail"
let idr_kw_raise        = Idr "raise"
let idr_2o_colon        = idr_2o_c ":"
let idr_2o_arrow        = idr_2o_c "→"
let idr_2o_arrowopt     = idr_2o_c "→?"
let idr_2o_times        = idr_2o_c "×"
let idr_2o_implies      = idr_2o_c "⇒"
let idr_2o_mapsto       = idr_2o_c "↦"
let idr_2o_index        = idr_2o_c "#"
let idr_2o_comma        = idr_2o_c ","
let idr_1o_semicolon    = idr_1o_c ";"
let idr_2o_semicolon    = idr_2o_c ";"
let idr_1o_qmark        = idr_1o_c "?"
let idr_2b_qmark        = idr_2b_c "?(" ")"
let idr_2o_vertical_bar = idr_2o_c "|"
let idr_2o_eq           = idr_2o_c "="
let idr_2o_coloneq      = idr_2o_c ":="
let idr_2o_A            = idr_2o_c "∀"
let idr_2o_E            = idr_2o_c "∃"
let idr_1o_not          = idr_1o_c "¬"
let idr_2o_and          = idr_2o_c "∧"
let idr_2o_or           = idr_2o_c "∨"
let idr_1q_functor      = idr_1q_c "Fun"
let idr_1b_paren        = idr_1b_c "(" ")"
let idr_1b_square_bracket = idr_1b_c "[" "]"
let idr_1b_curly_bracket = idr_1b_c "{" "}"
let idr_1b_array        = idr_1b_c "#[" "]"
let idr_2b_dotbracket   = idr_2b_c ".[" "]"
let idr_2b_dotparen     = idr_2b_c ".(" ")"
let idr_2b_as           = idr_2b_c "@(" ")"
let idr_1o_asterisk     = idr_1o_c "*"
let idr_effect          = Idr "__builtin_effect"
let idr_effect_run      = Idr "__builtin_effect_run"
let idr_effect_throw    = Idr "__builtin_effect_throw"
let idr_toplevel_run    = Idr "__builtin_toplevel_run"
let idr_catch           = Idr "__builtin_catch"
let idr_list_null       = Idr "[]"
let idr_list_push       = Idr "[;]"
let idr_set_null        = Idr "{}"
let idr_set_push        = Idr "{;}"
let idr_map_null        = Idr "{↦}"
let idr_map_push        = Idr "{↦;}"
let cidr_is_2o_colon    (Cidr (_, idr)) = idr = idr_2o_colon
let cidr_is_2o_arrow    (Cidr (_, idr)) = idr = idr_2o_arrow
let cidr_is_2o_arrowopt (Cidr (_, idr)) = idr = idr_2o_arrowopt
let cidr_is_2o_implies  (Cidr (_, idr)) = idr = idr_2o_implies
let cidr_is_2o_mapsto   (Cidr (_, idr)) = idr = idr_2o_mapsto
let cidr_is_2o_index    (Cidr (_, idr)) = idr = idr_2o_index
let cidr_is_2o_comma    (Cidr (_, idr)) = idr = idr_2o_comma
let cidr_is_1o_semicolon (Cidr (_, idr)) = idr = idr_1o_semicolon
let cidr_is_2o_semicolon (Cidr (_, idr)) = idr = idr_2o_semicolon
let cidr_is_1o_qmark    (Cidr (_, idr)) = idr = idr_1o_qmark
let cidr_is_2b_qmark    (Cidr (_, idr)) = idr = idr_2b_qmark
let cidr_is_2o_eq       (Cidr (_, idr)) = idr = idr_2o_eq
let cidr_is_2o_coloneq  (Cidr (_, idr)) = idr = idr_2o_coloneq
let cidr_is_1q_functor  (Cidr (_, idr)) = idr = idr_1q_functor
let cidr_is_1o_asterisk (Cidr (_, idr)) = idr = idr_1o_asterisk
let cidr_is_2b_dotbracket (Cidr (_, idr)) = idr = idr_2b_dotbracket
let cidr_is_2b_dotparen (Cidr (_, idr)) = idr = idr_2b_dotparen

let idrs_effect_tycon = [Idr "effect"; idr_2o_c "/~"]
let idr_is_effect_tycon idr = List.exists ((=) idr) idrs_effect_tycon
let cidr_is_effect_tycon (Cidr (_, idr)) = idr_is_effect_tycon idr

let idr_is_monad_op = function
  | Idr "do" -> true
  | _ -> false

let cmonad_io = ""

let cpred_loc = function
  | Cpred_let (loc, _, _, _, _)
  | Cpred_if (loc, _, _, _)
  | Cpred_back loc
  | Cpred_at (loc, _)
  | Cpred_expr0 (loc, _)
  | Cpred_expr (loc, _, _)
  | Cpred_expr_which (loc, _, _, _)
  | Cpred_seq (loc, _, _, _)
  | Cpred_seq_which (loc, _, _, _, _)
  | Cpred_cond (loc, _, _, _, _)
  | Cpred_upon (loc, _, _, _)
  -> loc

let ctrm_loc = function
  | Ctrm_literal (loc, _)
  | Ctrm_ref (Cidr (loc, _), _)
  | Ctrm_label (loc, _, _)
  | Ctrm_quantify (loc, _, _, _)
  | Ctrm_rel (loc, _, _)
  | Ctrm_apply (loc, _, _)
  | Ctrm_project (loc, _, _)
  | Ctrm_array (loc, _)
  | Ctrm_what (loc, _, _)
  | Ctrm_where (loc, _)
  | Ctrm_with (loc, _, _)
  -> loc

let cdef_loc = function
  | Cdef_include (loc, _, _)
  | Cdef_open (loc, _, _)
  | Cdef_use (loc, _)
  | Cdef_type (loc, _, _, _)
  | Cdef_in (loc, _, _, _)
  | Cdec_sig (loc, _)
  | Cdef_sig (loc, _, _)
  | Cdef_val (loc, _, _)
  | Cdef_let (loc, _, _, _)
  | Cdef_inj (loc, _, _)
  | Cdef_lex (loc, _, _)
  | Cdef_lexalias (loc, _)
  -> loc

let application_depth i f x =
  let rec loop n xs = function
    | Ctrm_apply (_, Ctrm_ref (Cidr (_, f'), _), x') when f' = f ->
        loop (n + 1) [] (List.nth (x' :: xs) i)
    | Ctrm_apply (_, f', x') ->
        loop n (x' :: xs) f'
    | _ -> n in
  loop 0 [] x
