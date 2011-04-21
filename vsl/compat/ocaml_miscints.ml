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

open Ocaml_unicode.Pervasive

module type A_basic_int = sig
    type t
    val neg : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val rem : t -> t -> t
    val of_int : int -> t
    val as_int : t -> int
    val show : t -> string
end

module Adapt
    (M : sig
	type t
	val neg : t -> t
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val rem : t -> t -> t
	val of_int : int -> t
	val to_int : t -> int
	val to_string : t -> uTF8string
    end) =
struct
    include M
    let as_int = to_int
    let show i = __string_of_utf8 (to_string i)
end

module Pervasive = struct
    module Nativeint = Adapt (Nativeint)
    module Int32 = Adapt (Int32)
    module Int64 = Adapt (Int64)
end
