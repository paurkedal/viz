(* Copyright 2011  Petter Urkedal
 *
 * This file is part of the Viz Standard Library <http://www.vizlang.org/>.
 *
 * The Viz Standard Library (VSL) is free software: you can redistribute it
 * and/or modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * The VSL is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the VSL.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ocaml_prereq

module Array_const : sig
    type 'a t = 'a array

    val length : 'a t -> int

    val get : int -> 'a t -> 'a

    val slice : int -> int -> 'a t -> 'a t

    val init : int -> (int -> 'a) -> 'a t

    val uniform : int -> 'a -> 'a t

    val of_list : 'a list -> 'a t

    val as_list : 'a t -> 'a list

    val map : ('a -> 'b) -> 'a t -> 'b t

    val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

    val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val foldr : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val cat : 'a t -> 'a t -> 'a t

    val cat_list : 'a t list -> 'a t
end

module Array_act : sig
    type 'a r

    val init : int -> (int -> 'a) -> ('f, 'a r) effect

    val uniform : int -> 'a -> ('f, 'a r) effect

    val get : int -> 'a r -> ('f, 'a) effect

    val slice : int -> int -> 'a r -> ('f, 'a r) effect

    val set : int -> 'a -> 'a r -> ('f, unit) effect

    val copy : 'a r -> ('f, 'a r) effect

    val fill : int -> int -> 'a -> 'a r -> ('f, unit) effect

    val blit : int -> int -> 'a r -> int -> 'a r -> ('f, unit) effect

    val map : ('a -> 'b) -> 'a r -> ('f, 'b r) effect

    val mapi : (int -> 'a -> 'b) -> 'a r -> ('f, 'b r) effect

    val fold : ('a -> 'b -> 'b) -> 'a r -> 'b -> ('f, 'b) effect

    val foldr : ('a -> 'b -> 'b) -> 'a r -> 'b -> ('f, 'b) effect

    val cat : 'a r -> 'a r -> ('f, 'a r) effect

    val cat_list : ('a r) list -> ('f, 'a r) effect

    val sort : ('a -> 'a -> torder) -> 'a r -> ('f, unit) effect

    val as_list : 'a r -> ('f, 'a list) effect

    val of_list : 'a list -> ('f, 'a r) effect

    val of_array : 'a array -> ('f, 'a r) effect

    val as_array : 'a r -> ('f, 'a array) effect

    (* val freeze : 'a r -> ('f, 'a array) effect *)
end
