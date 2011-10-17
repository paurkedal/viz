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
open Ocaml_unicode.Pervasive

module type A_basic_nat = sig
    type t

    val eq : t -> t -> bool
    val cmp : t -> t -> torder
    val op2_U2264 (* ≤ *) : t -> t -> bool
    val op2_U2265 (* ≥ *) : t -> t -> bool
    val op2_U003c (* < *) : t -> t -> bool
    val op2_U003e (* > *) : t -> t -> bool

    val width : int
    val zero : t
    val one : t

    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t   (* if signed, floored division *)
    val (mod) : t -> t -> t (* if signed, from floored division *)
    val quo : t -> t -> t   (* if signed, truncated division *)
    val rem : t -> t -> t   (* if signed, from truncated division *)

    val bitnot : t -> t
    val bitand : t -> t -> t
    val bitor : t -> t -> t
    val bitxor : t -> t -> t
    val shift : int -> t -> t

    val of_int : int -> t
    val as_int : t -> int
end

module type A_basic_int = sig
    include A_basic_nat

    val neg : t -> t
    val abs : t -> t
end

module Int : A_basic_int with type t = int
module Nint : A_basic_int with type t = nativeint
module Int32 : A_basic_int with type t = int32
module Int64 : A_basic_int with type t = int64
module Nat32 : A_basic_nat
module Nat64 : A_basic_nat

module Pervasive : sig
    type nint = nativeint
    type nativeint (* hidden *)
    type nat32 = Nat32.t
    type nat64 = Nat64.t
end
