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

external op1_U00ac : bool -> bool = "%boolnot"
external op2_U2227 : bool -> bool -> bool = "%sequand"
external op2_U2228 : bool -> bool -> bool = "%sequor"

type ('f, 'a) effect = { __unsafe_thunk : unit -> 'a; }
(** A monad type for sequencing actions.  The first type parameter is the
    "pocket" in which the effect is valid, and the second type parameter is
    the return type. *)

type world
(** A type tag for top-level "world" actions. *)

type 'a io = (world, 'a) effect
(** The top-level monad. *)

val __builtin_effect : (unit -> 'a) -> ('f, 'a) effect
(** Don't use this.  If it has any effect outside internal code, then there is
    a bug in some API. *)

val __builtin_effect_run : ('f, 'a) effect -> 'a
(** Don't use this.  It is reserved for compiler-generated code. *)

val __builtin_effect_return : 'a -> ('f, 'a) effect
val __builtin_effect_bind
    : ('a -> ('f, 'b) effect) -> ('f, 'a) effect -> ('f, 'b) effect

type exception__ = exn

val __builtin_raise : exception__ -> 'a

val __builtin_effect_throw : exception__ -> ('f, 'a) effect

val __builtin_catch : (exception__ -> 'a io) -> 'a io -> 'a io

val __builtin_mask : (<__it : 'a. 'a io -> 'a io> -> 'b io) -> 'b io
(* This type should be (((∀'a. 'a io -> 'a io) -> 'b io) -> 'b io) but we
 * don't rely on that for the standard definitions. *)

val __builtin_exit : int -> 'a io

module Ref : sig
    type ('f, 'a) r
    val init : 'a -> ('f, ('f, 'a) r) effect
    val get : ('f, 'a) r -> ('f, 'a) effect
    val set : ('f, 'a) r -> 'a -> ('f, unit) effect
end

val none : 'a option
val some : 'a -> 'a option

type torder = Tprec | Tcoin | Tsucc
val tprec : torder
val tcoin : torder
val tsucc : torder
val __adapt_cmp : ('a -> 'a -> torder) -> 'a -> 'a -> int
val __generic_eq : 'a -> 'a -> bool
val __generic_cmp : 'a -> 'a -> torder
