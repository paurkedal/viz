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

module type S = sig
    type 'r key
    type 'a t

    val empty : 'a t

    val is_empty : 'a t -> bool

    val head : 'a t -> 'a option

    val walk : 'r key -> 'a t -> 'a t

    val find : 'r key -> 'a t -> 'a option

    val singleton : 'r key -> 'a -> 'a t

    val add : 'r key -> 'a -> 'a t -> 'a t

    val remove : 'r key -> 'a t -> 'a t

    val prefix_fold : ('r key -> 'a -> 'b -> 'b) -> 'r key -> 'a t -> 'b -> 'b

    val prefix_optfold : ('r key -> 'a option -> 'b -> 'b)
		      -> 'r key -> 'a t -> 'b -> 'b
end