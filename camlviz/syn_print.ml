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
  let name = idr_to_string idr in
  Fo.put_string fo
    begin match idrhint with
    | Ih_none -> name
    | Ih_univ -> "%" ^ name
    | Ih_inj -> name ^ "%"
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
      if p < Opkind.p_apply then Fo.space fo;
      print_inline fo (Opkind.p_script 0) body
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
  | Ctrm_array (_, elts) ->
      Fo.put fo `Operator "#[";
      if elts <> [] then begin
        print_inline fo (Opkind.p_comma + 1) (List.hd elts);
        List.iter
          begin fun elt ->
            Fo.put fo `Operator ",";
            Fo.space fo;
            print_inline fo (Opkind.p_comma + 1) elt
          end
          (List.tl elts)
      end;
      Fo.put fo `Operator "]"
  | Ctrm_what (_, cm_opt, cpred) ->
      let kw = match cm_opt with None -> "what" | Some cm -> "what!" ^ cm in
      Fo.put_kw fo kw;
      print_predicate fo cpred
  | Ctrm_with (_, base, defs) ->
      Option.iter (print_inline fo p) base;
      Fo.put_kw fo "with";
      print_defs fo defs
  | Ctrm_where (_, defs) ->
      Fo.put_kw fo "where";
      print_defs fo defs

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
  | Cpred_back _ ->
      Fo.put fo `Comment "{# backtrack #}"
  | Cpred_at (_, cases) ->
      List.iter (fun (pat, cq) ->
        Fo.newline fo;
        Fo.put_kw fo "at";
        print_inline fo Opkind.p_min pat;
        Fo.enter_indent fo;
        print_predicate fo cq;
        Fo.leave_indent fo;
      ) cases
  | Cpred_expr0 (_, Idr verb) ->
      Fo.newline fo;
      Fo.put_kw fo verb
  | Cpred_expr (_, Idr verb, ctrm) ->
      Fo.newline fo;
      Fo.put_kw fo verb;
      print_inline fo Opkind.p_min ctrm
  | Cpred_expr_which (_, Idr verb, ctrm, (mw_opt, cw)) ->
      Fo.newline fo;
      Fo.put_kw fo verb;
      print_inline fo Opkind.p_min ctrm;
      Fo.put_kw fo
        (match mw_opt with None -> "which" | Some mw -> ("which!" ^ mw));
      Fo.enter_indent fo;
      print_predicate fo cw;
      Fo.leave_indent fo
  | Cpred_seq (_, Idr op, cx, cy_opt) ->
      Fo.newline fo;
      Fo.put_kw fo op;
      print_inline fo Opkind.p_min cx;
      Option.iter (print_predicate fo) cy_opt
  | Cpred_seq_which (_, Idr op, cx, (cm_opt, cw), cy_opt) ->
      Fo.newline fo;
      Fo.put_kw fo op;
      print_inline fo Opkind.p_min cx;
      Fo.put_kw fo
        (match cm_opt with None -> "which" | Some cm -> ("which!" ^ cm));
      Fo.enter_indent fo;
      print_predicate fo cw;
      Fo.leave_indent fo;
      Option.iter (print_predicate fo) cy_opt;
  | Cpred_cond (_, Idr op, cx, cy, cz_opt) ->
      Fo.newline fo;
      Fo.put_kw fo op;
      print_inline fo Opkind.p_min cx;
      Fo.enter_indent fo;
      print_predicate fo cy;
      Fo.leave_indent fo;
      Option.iter (print_predicate fo) cz_opt
  | Cpred_upon (_, cx, handler, thunk) ->
      Fo.newline fo;
      Fo.put_kw fo "upon";
      print_inline fo Opkind.p_min cx;
      Fo.enter_indent fo;
      print_predicate fo handler;
      Fo.leave_indent fo;
      print_predicate fo thunk

and print_def fo cdef =
  Fo.newline fo;
  match cdef with
  | Cdef_include (_, gen, path) ->
      Fo.put_kw fo (if gen then "include!" else "include");
      print_inline fo Opkind.p_min path
  | Cdef_open (_, abi, path) ->
      let kw = match abi with Abi_Viz -> "open" | Abi_C -> "open/c" in
      Fo.put_kw fo kw;
      print_inline fo Opkind.p_min path
  | Cdef_use (_, x) ->
      Fo.put_kw fo "use";
      print_inline fo Opkind.p_min x
  | Cdef_type (_, Abi_Viz, eqn, defs) ->
      Fo.put_kw fo "type";
      print_inline fo Opkind.p_min eqn;
      if defs <> [] then print_defs fo defs
  | Cdef_type (_, Abi_C, eqn, defs) ->
      Fo.put_kw fo "type:c";
      print_inline fo Opkind.p_min eqn;
      if defs <> [] then print_defs fo defs
  | Cdef_in (_, gen, pat, body) ->
      Fo.put_kw fo (if gen then "in!" else "in");
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
  | Cdef_val (_, (expo, abi, valopts), typing) ->
      let kw = (match abi with Abi_Viz -> "val" | Abi_C -> "val/c")
             ^ (if List.mem `Is_finalizer valopts then "f" else "")
             ^ (if List.mem `Is_stub valopts then "s" else "")
             ^ (match expo with `Local -> "-" | _ -> "") in
      Fo.put_kw fo kw;
      print_inline fo Opkind.p_min typing
  | Cdef_let (_, cm_opt, pat, pred) ->
      let kw = Option.fold (fun cm kw -> kw ^ "!" ^ cm) cm_opt "let" in
      Fo.put_kw fo kw;
      print_inline fo Opkind.p_min pat;
      Fo.enter_indent fo;
      print_predicate fo pred;
      Fo.leave_indent fo
  | Cdef_inj (_, abi, typing) ->
      Fo.put_kw fo (match abi with Abi_Viz -> "inj" | Abi_C -> "inj:c");
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
  | Cdef_lexalias (_, op_pairs) ->
      Fo.put_kw fo "lexalias";
      List.iter
        (fun (a, b) ->
          Fo.space fo; Fo.put fo `Operator (cidr_to_string a);
          Fo.space fo; Fo.put fo `Operator (cidr_to_string b))
        op_pairs
and print_defs fo defs =
  Fo.enter_indent fo;
  List.iter (print_def fo) defs;
  Fo.leave_indent fo

let print fo =
  print_inline fo Opkind.p_min

let cpred_to_string cpred =
  let fo = Formatter.create () in
  print_predicate fo cpred;
  Formatter.contents fo

let ctrm_to_string ctrm =
  let fo = Formatter.create () in
  print fo ctrm;
  Formatter.contents fo

let cdef_to_string cdef =
  let fo = Formatter.create () in
  print_def fo cdef;
  Formatter.contents fo
