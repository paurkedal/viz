(* Copyright 2011  Petter Urkedal
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

open Leaf_types
open Leaf_core
open Ast_types
open FfPervasives

type binding = loc * avar * atyp option * aval

module Bindings_graph = struct
    type graph = (Idr_set.t * binding) Idr_map.t
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
	| `Value, Apath ([], Avar (_, idr)) -> f idr
	| _ -> ident in
    Ast_utils.fold_aval_paths f' aval

let collect_binding_components bindings adefs =
    (* Build dependency graph. *)
    let add_vertex ((_, Avar (_, v), _, aval) as binding) (g, vs) =
	let deps = fold_aval_deps Idr_set.add aval Idr_set.empty in
	let g = Idr_map.add v (deps, binding) g in
	(g, v :: vs) in
    let (g, vs) = List.fold add_vertex bindings (Bindings_graph.empty, []) in
    let vs = List.rev vs in

    (* Remove dependecies not included in g. *)
    let g = Idr_map.map
	(fun (vs, binding) ->
	    (Idr_set.filter (fun v -> Idr_map.mem v g) vs, binding)) g in

    (* Report components. *)
    let vertex_deps v    = fst (Idr_map.find v g) in
    let vertex_binding v = snd (Idr_map.find v g) in
    let push_component vs adefs =
	match vs with
	| [v] when not (Idr_set.mem v (vertex_deps v)) ->
	    let loc, var, topt, body = vertex_binding v in
	    let pat = Option.fold (fun t v -> Apat_intype (loc, t, v)) topt
				  (Apat_uvar var) in
	    Adef_let (loc, pat, body) :: adefs
	| _ ->
	    Adef_letrec (List.map vertex_binding vs) :: adefs in
    Bindings_algo.fold_strongly_connected g push_component vs adefs
