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
    val minimum : t
    val maximum : t

    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val (mod) : t -> t -> t
    val quo : t -> t -> t
    val rem : t -> t -> t
    val abs : t -> t

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
end

module type Ocaml_int = sig
    type t
    val zero : t
    val one : t
    val min_int : t
    val max_int : t
    val neg : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val rem : t -> t -> t
    val logand : t -> t -> t
    val logor : t -> t -> t
    val logxor : t -> t -> t
    val lognot : t -> t
    val shift_left : t -> int -> t
    val shift_right : t -> int -> t
    val shift_right_logical : t -> int -> t
    val abs : t -> t
    val of_int : int -> t
    val to_int : t -> int
end

module Adapt (M : Ocaml_int) = struct
    include M

    let eq x y = x = y
    let cmp = __generic_cmp
    let op2_U2264 (* ≤ *) (x : t) (y : t) = x <= y
    let op2_U2265 (* ≥ *) (x : t) (y : t) = x >= y
    let op2_U003c (* < *) (x : t) (y : t) = x < y
    let op2_U003e (* > *) (x : t) (y : t) = x > y

    let width =
	let rec loop n x =
	    if x = zero then n else
	    loop (n + 1) (shift_left x 1) in
	loop 0 one

    let minimum = min_int
    let maximum = max_int

    let quo = M.div
    let rem = M.rem

    let div x y =
	let (+) = add and (-) = sub in
	match x < zero, y < zero with
	| false, false | true, true -> quo x y
	| true, false -> quo (x - y + one) y
	| false, true -> quo (x - y - one) y

    let (mod) x y =
	let (+) = add and (-) = sub in
	match x < zero, y < zero with
	| false, false | true, true -> rem x y
	| true, false -> rem (x + one) y + y - one
	| false, true -> rem (x - y - one) y + y + one

    let bitnot = lognot
    let bitand = logand
    let bitor = logor
    let bitxor = logxor
    let shift i x =
	if i >= 0 then shift_left x i else
	shift_right x (- i)

    let as_int = to_int
end

module Int = struct
    type t = int

    let zero = 0
    let one = 1
    let minimum = min_int
    let maximum = max_int

    let eq x y = x = y
    let cmp = __generic_cmp
    let op2_U2264 (* ≤ *) (x : t) (y : t) = x <= y
    let op2_U2265 (* ≥ *) (x : t) (y : t) = x >= y
    let op2_U003c (* < *) (x : t) (y : t) = x < y
    let op2_U003e (* > *) (x : t) (y : t) = x > y

    let width =
	let rec loop n x =
	    if x = zero then n else
	    loop (n + 1) (x lsl 1) in
	loop 0 one

    let add = Pervasives.(+)
    let sub = Pervasives.(-)
    let mul = Pervasives.( * )
    let quo = Pervasives.(/)
    let rem = Pervasives.(mod)
    let div x y =
	match x < zero, y < zero with
	| false, false | true, true -> quo x y
	| true, false -> quo (x - y + one) y
	| false, true -> quo (x - y - one) y
    let (mod) x y =
	match x < zero, y < zero with
	| false, false | true, true -> rem x y
	| true, false -> rem (x + one) y + y - one
	| false, true -> rem (x - y - one) y + y + one

    let bitnot = Pervasives.lnot
    let bitand = Pervasives.(land)
    let bitor = Pervasives.(lor)
    let bitxor = Pervasives.(lxor)
    let shift i x = if i >= 0 then x lsl i else x asr (- i)

    let of_int x = x
    let as_int x = x

    let neg = Pervasives.(~-)
    let abs = abs
end

module Nat32 = struct
    type t = int32

    let eq x y = x = y
    external cmp : t -> t -> torder = "cviz_nat32_cmp"
    let op2_U2264 (* ≤ *) (x : t) (y : t) = cmp x y <> Tsucc
    let op2_U2265 (* ≥ *) (x : t) (y : t) = cmp x y <> Tprec
    let op2_U003c (* < *) (x : t) (y : t) = cmp x y = Tprec
    let op2_U003e (* > *) (x : t) (y : t) = cmp x y = Tsucc

    let width = 32
    let zero = Int32.zero
    let one = Int32.one
    let minimum = Int32.of_int 0
    let maximum = Int32.of_int (-1)

    let add = Int32.add
    let sub = Int32.sub
    external mul : t -> t -> t = "cviz_nat32_mul"
    external div : t -> t -> t = "cviz_nat32_div"
    external (mod) : t -> t -> t = "cviz_nat32_mod"
    let quo = div
    let rem = (mod)
    let abs x = x

    let bitnot = Int32.lognot
    let bitand = Int32.logand
    let bitor = Int32.logor
    let bitxor = Int32.logxor
    let shift i x =
	if i >= 0 then Int32.shift_left x i else
	Int32.shift_right_logical x (- i)

    external as_int : t -> int = "cviz_nat32_as_int"
    external of_int : int -> t = "cviz_nat32_of_int"
end

module Nat64 = struct
    type t = int64

    let eq x y = x = y
    external cmp : t -> t -> torder = "cviz_nat64_cmp"
    let op2_U2264 (* ≤ *) (x : t) (y : t) = cmp x y <> Tsucc
    let op2_U2265 (* ≥ *) (x : t) (y : t) = cmp x y <> Tprec
    let op2_U003c (* < *) (x : t) (y : t) = cmp x y = Tprec
    let op2_U003e (* > *) (x : t) (y : t) = cmp x y = Tsucc

    let width = 64
    let zero = Int64.zero
    let one = Int64.one
    let minimum = Int64.of_int 0
    let maximum = Int64.of_int (-1)

    let add = Int64.add
    let sub = Int64.sub
    external mul : t -> t -> t = "cviz_nat64_mul"
    external div : t -> t -> t = "cviz_nat64_div"
    external (mod) : t -> t -> t = "cviz_nat64_mod"
    let quo = div
    let rem = (mod)
    let abs x = x

    let bitnot = Int64.lognot
    let bitand = Int64.logand
    let bitor = Int64.logor
    let bitxor = Int64.logxor
    let shift i x =
	if i >= 0 then Int64.shift_left x i else
	Int64.shift_right_logical x (- i)

    external as_int : t -> int = "cviz_nat64_as_int"
    external of_int : int -> t = "cviz_nat64_of_int"
end

module Nint = Adapt (Nativeint)
module Int32 = Adapt (Int32)
module Int64 = Adapt (Int64)

module Pervasive = struct
    type nint = nativeint
    type nativeint = unit
    type nat32 = Nat32.t
    type nat64 = Nat64.t
end
