(* Copyright 2011  Petter Urkedal
 *
 * This file is part of the Fform Standard Library.
 *
 * The Fform Standard Library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * Fform is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Fform.  If not, see <http://www.gnu.org/licenses/>.
 *)

type ('f, 'a) action
(** A monad type for sequencing actions.  The first type parameter is the
    "pocket" in which the action is valid, and the second type parameter is
    the return type. *)

type world_pocket
(** A type tag for top-level "world" actions. *)

type 'a io = (world_pocket, 'a) action
(** The top-level monad. *)

val __unsafe_action : (unit -> 'a) -> ('f, 'a) action
(** Don't use this.  If it has any effect outside internal code, then there is
    a bug in some API. *)

val __unsafe_run_action : ('f, 'a) action -> 'a
(** Don't use this.  It is reserved for compiler-generated code. *)

val __builtin_action_return : 'a -> ('f, 'a) action
val __builtin_action_bind
    : ('a -> ('f, 'b) action) -> ('f, 'a) action -> ('f, 'b) action

val none : 'a option
val some : 'a -> 'a option

type torder = Tprec | Tcoin | Tsucc
val tprec : torder
val tcoin : torder
val tsucc : torder
val __adapt_cmp : ('a -> 'a -> torder) -> 'a -> 'a -> int
