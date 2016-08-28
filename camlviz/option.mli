(* Copyright (C) 2010--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

(** A Structure for the [option] Type *)

val get : 'a option -> 'a
val default : 'a -> 'a option -> 'a
val default_opt : 'a option -> 'a option -> 'a option
val fold : ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
val iter : ('a -> unit) -> 'a option -> unit
val map : ('a -> 'b) -> 'a option -> 'b option
val map_fold : ('a * 'c -> 'b * 'c) -> 'a option * 'c -> 'b option * 'c
val for_all : ('a -> bool) -> 'a option -> bool
val exists  : ('a -> bool) -> 'a option -> bool
val filter  : ('a -> bool) -> 'a option -> 'a option
