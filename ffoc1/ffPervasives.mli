(* Copyright 2010--2011  Petter Urkedal
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

(** Pervasive Additions *)

open CamomileLibrary.Default.Camomile
include module type of UPervasives

exception Unimplemented

val ident : 'a -> 'a
val konst : 'a -> 'b -> 'a
val uncurry : ('a -> 'b -> 'g) -> 'a * 'b -> 'g
val curry : ('a * 'b -> 'g) -> 'a -> 'b -> 'g
val repeat : int -> ('a -> 'a) -> 'a -> 'a

val ( *< ) : ('b -> 'g) -> ('a -> 'b) -> 'a -> 'g
val ( *> ) : ('a -> 'b) -> ('b -> 'g) -> 'a -> 'g
val ( @< ) : ('a -> 'b) -> 'a -> 'b

val int_of_digit : char -> int

module List : sig
    include module type of List

    val push : 'a -> 'a list -> 'a list

    val last : 'a list -> 'a

    val compare_with : ('a -> 'a -> int) -> 'a list -> 'a list -> int

    val init : int -> (int -> 'a) -> 'a list

    val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

    val map_fold : ('a * 'c -> 'b * 'c) -> 'a list * 'c -> 'b list * 'c

    val rev_filter : ('a -> bool) -> 'a list -> 'a list

    val find_image : ('a -> 'b option) -> 'a list -> 'b option

    val split_before : ('a -> bool) -> 'a list -> 'a list * 'a list

    val split_after : ('a -> bool) -> 'a list -> 'a list * 'a list

    val map_while : ('a -> 'b option) -> 'a list -> 'a list * 'b list

    val drop_while : ('a -> bool) -> 'a list -> 'a list
end

module Char : sig
    include module type of Char

    val is_space : char -> bool
    val is_lower : char -> bool
    val is_upper : char -> bool
end

module String : sig
    include module type of String

    val map_of_list : ('a -> char) -> 'a list -> string

    val split_on_char : char -> string -> string list

    val after : int -> string -> string

    val starts_with : string -> string -> bool
end
