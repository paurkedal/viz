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

(** Operations on Generic Sequences *)

type ('a, 'r) t = ('r -> ('a * 'r) option) * 'r

val pop : ('a, 'r) t -> ('a * ('a, 'r) t) option

val peek : ('a, 'r) t -> 'a option

val fold : ('a -> 'b -> 'b) -> ('a, 'r) t -> 'b -> 'b

val iter : ('a -> unit) -> ('a, 'r) t -> unit

val iter_n : ('a -> unit) -> int -> ('a, 'r) t -> unit

val of_list : 'a list -> ('a, 'a list) t

val to_list : ('a, 'r) t -> 'a list
