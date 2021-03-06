(* Copyright (C) 2011--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Prereq
open Ast_types
open Ast_core
open Cst_core

let apat_pair loc x y =
  Apat_apply (loc, Alabel_none,
    Apat_apply (loc, Alabel_none,
      Apat_ref (Apath (loc, Modpath.atom idr_2o_comma)),
      x),
    y)

let aval_pair loc x y =
  Aval_apply (loc, Alabel_none,
    Aval_apply (loc, Alabel_none,
      Aval_ref (Apath (loc, Modpath.atom idr_2o_comma)),
      x),
    y)

let rec std_aval_rewrite = function
  | Aval_at (locA, None, casesA) as self ->
    let case_is_atexpr = function
      | (_, None, Aval_at _) -> true
      | _ -> false in
    if not (List.for_all case_is_atexpr casesA) then self else
    let varA = fresh_avar_at ~prefix:"_a" locA in
    let varB = fresh_avar_at ~prefix:"_b" locA in
    let paired_cases =
      List.map begin function
        | (patA, None, Aval_at (locB, None, casesB)) ->
            List.map begin fun (patB, condB, retB) ->
                (apat_pair locB patA patB, condB, retB)
            end casesB
        | _ -> assert false
      end casesA
      |> List.flatten in
    let argA = Aval_ref (apath_of_avar varA) in
    let argB = Aval_ref (apath_of_avar varB) in
    let inner = std_aval_rewrite (Aval_at (locA, None, paired_cases)) in
    Aval_at (locA, None, [Apat_uvar varA, None,
      Aval_at (locA, None, [Apat_uvar varB, None,
        Aval_apply (locA, Alabel_none,
                    inner, aval_pair locA argA argB)])])
  | self -> self

let rec std_amod_rewrite m =
  m |> amod_map_subamod std_amod_rewrite |> amod_map_subaval std_aval_rewrite
