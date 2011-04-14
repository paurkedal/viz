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

open FfPervasives

module type ST_GRAPH = sig
    type graph
    type vertex

    module Vertex_map : sig
	type 'a t
	val empty : 'a t
	val add : vertex -> 'a -> 'a t -> 'a t
	val mem : vertex -> 'a t -> bool
	val find : vertex -> 'a t -> 'a
    end

    val fold_adjacent : graph -> (vertex -> 'a -> 'a) -> vertex -> 'a -> 'a
end

module ST_algo (G : ST_GRAPH) = struct

    module VM = G.Vertex_map

    let fold_strongly_connected g f vs accu =
	(* The Cheriyan-Mehlhorn/Gabow algorithm for partitioning graph vertices
	 * into the strongly connected components.
	 *
	 * In the following, [preord : int option VM.t] maps to [None] if
	 * the key has been assigned to a component, otherwise it maps to [Some
	 * c] where [c] is the preorder number. *)
	let rec process_vertex v (c, s, p, preord, accu) =
	    let preord = VM.add v (Some c) preord in
	    let (c, s, p, preord, accu) =
		G.fold_adjacent g process_adjacent_vertex v
			(c + 1, v :: s, v :: p, preord, accu) in
	    if List.hd p == v then
		let (comp, s) = List.split_after ((==) v) s in
		let preord = List.fold (fun u -> VM.add u None) comp preord in
		(c, s, List.tl p, preord, f comp accu) else
	    (c, s, p, preord, accu)
	and process_adjacent_vertex w (c, s, p, preord, accu) =
	    try match VM.find w preord with
	    | None ->		(* Assigend to a component. *)
		(c, s, p, preord, accu)
	    | (Some cw) ->	(* Not assigned to a component. *)
		let after_w u =
		    match VM.find u preord with
		    | Some cu -> cu > cw
		    | _ -> assert false in
		let p = List.drop_while after_w p in
		(c, s, p, preord, accu)
	    with Not_found ->	(* Not yet seen. *)
		process_vertex w (c, s, p, preord, accu) in
	let process_new_vertex v (c, s, p, preord, accu) =
	    if VM.mem v preord then (c, s, p, preord, accu) else
	    process_vertex v (c, s, p, preord, accu) in
	let (_, _, _, _, accu') =
	    List.fold process_new_vertex vs (0, [], [], VM.empty, accu) in
	accu'
end


module type VERTEX = sig
    module Key : Map.OrderedType

    type t

    val key : t -> Key.t
    val fold_adjs : (t -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make (Vertex : VERTEX) = struct

    module Key_map = struct
	include Map.Make (Vertex.Key)

	let add_vertex v = add (Vertex.key v)
	let mem_vertex v = mem (Vertex.key v)
	let find_vertex_e v = find (Vertex.key v)
	let find_vertex v m =
	    try Some (find_vertex_e v m) with Not_found -> None
    end

    let fold_strongly_connected f vs accu =
	(* The Cheriyan-Mehlhorn/Gabow algorithm for partitioning graph vertices
	 * into the strongly connected components.
	 *
	 * In the following, [preord : int option Key_map.t] maps to [None] if
	 * the key has been assigned to a component, otherwise it maps to [Some
	 * c] where [c] is the preorder number. *)
	let rec process_vertex v (c, s, p, preord, accu) =
	    let preord = Key_map.add_vertex v (Some c) preord in
	    let (c, s, p, preord, accu) =
		Vertex.fold_adjs process_adjacent_vertex v
			(c + 1, v :: s, v :: p, preord, accu) in
	    if List.hd p == v then
		let (comp, s) = List.split_after ((==) v) s in
		let preord = List.fold (fun u -> Key_map.add_vertex u None)
				       comp preord in
		(c, s, List.tl p, preord, f comp accu) else
	    (c, s, p, preord, accu)
	and process_adjacent_vertex w (c, s, p, preord, accu) =
	    match Key_map.find_vertex w preord with
	    | None ->		(* Not yet seen. *)
		process_vertex w (c, s, p, preord, accu)
	    | Some None ->	(* Assigend to a component. *)
		(c, s, p, preord, accu)
	    | Some (Some cw) ->	(* Not assigned to a component. *)
		let after_w u =
		    match Key_map.find_vertex_e u preord with
		    | Some cu -> cu > cw
		    | _ -> assert false in
		let p = List.drop_while after_w p in
		(c, s, p, preord, accu) in
	let process_new_vertex v (c, s, p, preord, accu) =
	    if Key_map.mem_vertex v preord then (c, s, p, preord, accu) else
	    process_vertex v (c, s, p, preord, accu) in
	let (_, _, _, _, accu') =
	    List.fold process_new_vertex vs (0, [], [], Key_map.empty, accu) in
	accu'
end
