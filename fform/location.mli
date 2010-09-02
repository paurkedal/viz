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

open Unicode

module Bound : sig
    type t

    val init : string -> t

    val dummy : t

    val path : t -> string

    val lineno : t -> int

    val charno : t -> int

    val column : t -> int

    val skip_n : int -> t -> t

    val skip_tab : t -> t

    val skip_newline : t -> t

    val skip_char : UChar.t -> t -> t

    val to_string : t -> string

    (** Convert from [Lexing.position].  Note that the result will not have a
     ** column number. *)
    val of_lexing_position : Lexing.position -> t

    (** Convert to [Lexing.position].  Not that this throws away the column
     ** number. *)
    val to_lexing_position : t -> Lexing.position
end

type t

val between : Bound.t -> Bound.t -> t

val at : Bound.t -> t

val dummy : t

val path : t -> string

val lbound : t -> Bound.t

val ubound : t -> Bound.t

val to_string : t -> string
