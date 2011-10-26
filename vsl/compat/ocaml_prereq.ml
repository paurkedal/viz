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

(* Dark spells about the world state. *)
type ('f, 'a) effect = { __unsafe_thunk : unit -> 'a; }
type 'a _any_effect = { _any_effect : 'f. ('f, 'a) effect; }
type world
type 'a io = (world, 'a) effect
let __builtin_effect f = { __unsafe_thunk = f; }
let __builtin_effect_run m = m.__unsafe_thunk ()

let __builtin_effect_return x = { __unsafe_thunk = fun () -> x; }
let __builtin_effect_bind k m =
    let f () = __builtin_effect_run (k (__builtin_effect_run m)) in
    { __unsafe_thunk = f; }

type exception__ = exn

let __builtin_raise e = raise e

let __builtin_effect_throw e = __builtin_effect (fun () -> raise e)

let __builtin_catch k m =
    let f () = try __builtin_effect_run m with e -> __builtin_effect_run (k e) in
    { __unsafe_thunk = f; }

type _A_ioio = { _A_ioio : 'a. 'a io -> 'a io }

let __builtin_mask f = f { _A_ioio = fun m -> m }  (* No async exceptions. *)

let __builtin_exit err = { __unsafe_thunk = (fun () -> exit err) }

(* References *)
module Ref = struct
    type ('f, 'a) r = 'a ref
    let init x = __builtin_effect (fun () -> ref x)
    let get r = __builtin_effect (fun () -> !r)
    let set r x = __builtin_effect (fun () -> r := x)
end

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
