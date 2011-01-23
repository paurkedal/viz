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

module ST_algo (G : ST_GRAPH) : sig
    val fold_strongly_connected
	: G.graph -> (G.vertex list -> 'a -> 'a) -> G.vertex list -> 'a -> 'a
end
