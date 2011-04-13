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

external op1_U00ac : bool -> bool = "%boolnot"
external op2_U2227 : bool -> bool -> bool = "%sequand"
external op2_U2228 : bool -> bool -> bool = "%sequor"

(* Dark spells about the world state. *)
type ('f, 'a) action = { __unsafe_thunk : unit -> 'a; }
type world
type 'a io = (world, 'a) action
let __unsafe_action f = { __unsafe_thunk = f; }
let __unsafe_run_action m = m.__unsafe_thunk ()

let __builtin_action_return x = { __unsafe_thunk = fun () -> x; }
let __builtin_action_bind k m =
    let f () = __unsafe_run_action (k (__unsafe_run_action m)) in
    { __unsafe_thunk = f; }

(* Options *)
let none = None
let some x = Some x

(* A total order. Make sure the constructor order is in sync with C stubs. *)
type torder = Tprec | Tcoin | Tsucc
let tprec = Tprec
let tcoin = Tcoin
let tsucc = Tsucc
let __adapt_cmp cmp x y =
    match cmp x y with
    | Tprec -> -1
    | Tcoin -> 0
    | Tsucc -> 1
let __generic_cmp x y =
    let cmp = Pervasives.compare x y in
    if cmp < 0 then Tprec else
    if cmp > 0 then Tsucc else
    Tcoin
let __generic_eq x y = x = y
