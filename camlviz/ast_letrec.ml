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

open Leaf_types
open Leaf_core
open Ast_types
open FfPervasives
open Diag

type binding = loc * avar * atyp option * aval

let binding_loc (loc, _, _, _) = loc

module Bindings_graph = struct
  type graph = (Idr_set.t * binding list) Idr_map.t
  type vertex = idr
  module Vertex_map = Idr_map

  let empty : graph = Idr_map.empty

  let fold_adjacent (g : graph) f v =
    try
      let (vs, _) = Idr_map.find v g in
      Idr_set.fold f vs
    with Not_found -> ident
end

module Bindings_algo = Graphalgo.ST_algo (Bindings_graph)

let fold_aval_deps f aval =
  let f' stratum path =
    match (stratum, path) with
    | `Value, Apath (_, p) when Modpath.is_atom p -> f (Modpath.last_e p)
    | _ -> ident in
  Ast_utils.fold_aval_paths f' aval

let collect_binding_components bindings adefs =
  (* Build dependency graph. *)
  let add_vertex ((loc, Avar (_, v), _, aval) as binding) (g, vs) =
    let deps = fold_aval_deps Idr_set.add aval Idr_set.empty in
    let deps, bindings =
      if not (Idr_map.mem v g) then (deps, [binding]) else
      let deps', bindings' = Idr_map.find v g in
      (Idr_set.union deps deps', binding :: bindings') in
    let g = Idr_map.add v (deps, bindings) g in
    (g, v :: vs) in
  let (g, vs) = List.fold add_vertex bindings (Bindings_graph.empty, []) in
  let vs = List.rev vs in

  (* Remove dependecies not included in g. *)
  let g = Idr_map.map
    (fun (vs, bindings) ->
      (Idr_set.filter (fun v -> Idr_map.mem v g) vs, bindings)) g in

  (* Report components. *)
  let vertex_bindings v = snd (Idr_map.find v g) in
  let vertex_binding v =
    match vertex_bindings v with
    | [binding] -> binding
    | bindings ->
        let loc = Textloc.span (List.map binding_loc bindings) in
        errf_at loc "Mixing recursion and shadowing is not supported." in
  let push_component vs adefs =
    match vs with
    | [v] when
            begin match Idr_map.find v g with
            | deps, [binding] -> not (Idr_set.mem v deps)
            | _ -> true
            end ->
        List.fold
            begin fun (loc, var, topt, body) adefs ->
                let pat = Option.fold
                            (fun t v -> Apat_intype (loc, t, v)) topt
                            (Apat_uvar var) in
                Adef_let (loc, pat, body) :: adefs
            end
            (List.rev (vertex_bindings v)) adefs
    | _ ->
        Adef_letrec (List.map vertex_binding vs) :: adefs in
  Bindings_algo.fold_strongly_connected g push_component vs adefs
