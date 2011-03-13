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

(* Dark spells about the world state. *)
type ('f, 'a) action = Unsafe_thunk of (unit -> 'a)
type world_pocket
type 'a io = (world_pocket, 'a) action
let __unsafe_action f = Unsafe_thunk f
let __unsafe_run_action (Unsafe_thunk f) = f ()

let __builtin_action_return x = Unsafe_thunk (fun () -> x)
let __builtin_action_bind k (Unsafe_thunk f) =
    Unsafe_thunk (fun () -> __unsafe_run_action (k (f ())))

(* Options *)
let none = None
let some x = Some x

type torder = Tprec | Tcoin | Tsucc
let tprec = Tprec
let tcoin = Tcoin
let tsucc = Tsucc
let __adapt_cmp cmp x y =
    match cmp x y with
    | Tprec -> -1
    | Tcoin -> 0
    | Tsucc -> 1
