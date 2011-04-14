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

(** Algorithms on Graph-Like Data *)

(** The signature of a "simply traversable" graph, without edges but with an
    associated map structure for the vertices. *)
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

(** Functor to instantiate the algorithms on [ST_GRAPH]s. *)
module ST_algo (G : ST_GRAPH) : sig
    val fold_strongly_connected
	: G.graph -> (G.vertex list -> 'a -> 'a) -> G.vertex list -> 'a -> 'a
    (** [fold_strongly_connected g f vs] is the composition of [f us] for each
	[us] in the partition of strongly connected components of [vs].  The
	components are passed to [f] in topological order, starting with the
	deepest vertices as the rightmost factor.  The implementation follows
	the Cheriyan-Mehlhorn/Gabow algorithm. *)
end
