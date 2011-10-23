(* Copyright 2010--2011  Petter Urkedal
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

(** Source Code Location *)

open Unicode

val string_of_lineno : int -> string

val string_of_colno : int -> string

module Bound : sig
    (** A Source Code Location Boundary (Point) *)

    type t

    val init : string -> t

    val init_lc : string -> int -> int -> t

    val dummy : t

    val path : t -> string

    val lineno : t -> int

    val charno : t -> int

    val bol_charno : t -> int

    val column : t -> int

    val skip_n : int -> t -> t

    val skip_tab : t -> t

    val skip_newline : t -> t

    val skip_char : UChar.t -> t -> t

    val compare : t -> t -> int

    val min : t -> t -> t

    val max : t -> t -> t

    val to_string : t -> string

    (** Convert from [Lexing.position].  Note that the result will not have a
	column number. *)
    val of_lexing_position : Lexing.position -> t

    (** Convert to [Lexing.position].  Not that this throws away the column
	number. *)
    val to_lexing_position : t -> Lexing.position
end

type t

val between : Bound.t -> Bound.t -> t

val span : t list -> t

val at : Bound.t -> t

val dummy : t

val path : t -> string

val lbound : t -> Bound.t

val ubound : t -> Bound.t

val to_string : t -> string
