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

(** A Lookahead Stream Used by the Lexer *)

open Unicode

type t
type elt = UChar.t

val null : t

val open_in : string -> t

val locbound : t -> Location.Bound.t

val pop : t -> elt option

val peek : t -> elt option

val peek_n : int -> t -> elt list

val peek_at : int -> t -> elt option

val pop_code : t -> int

val peek_code : t -> int

val peek_n_code : int -> t -> int list

val skip : t -> unit

val skip_n : int -> t -> unit

val skip_while : (elt -> bool) -> t -> unit

val scan_while : (elt -> bool) -> t -> UString.t * Location.t
