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

open CamomileLibrary.Default.Camomile
include module type of UPervasives

exception Unimplemented

val ident : 'a -> 'a
val konst : 'a -> 'b -> 'a
val uncurry : ('a -> 'b -> 'g) -> 'a * 'b -> 'g
val curry : ('a * 'b -> 'g) -> 'a -> 'b -> 'g

val ( *< ) : ('b -> 'g) -> ('a -> 'b) -> 'a -> 'g
val ( *> ) : ('a -> 'b) -> ('b -> 'g) -> 'a -> 'g
val ( @< ) : ('a -> 'b) -> 'a -> 'b

val int_of_digit : char -> int

module List : sig
    include module type of List

    val push : 'a -> 'a list -> 'a list

    val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

    val find_image : ('a -> 'b option) -> 'a list -> 'b option
end

module Char : sig
    include module type of Char

    val is_space : char -> bool
end

module String : sig
    include module type of String

    val split_on_char : char -> string -> string list
end
