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

module Array_const = struct
    type 'a t = 'a array

    let length = Array.length
    let get i a = Array.get a i
    let slice n i a = Array.sub a i n
    let init n f = Array.init n f
    let init_fold n f accu =
	let accu_r = ref accu in
	let f' i =
	    let x, accu = f !accu_r in
	    accu_r := accu;
	    x in
	let a = init n f' in
	(a, !accu_r)
    let of_list = Array.of_list
    let as_list = Array.to_list
    let cat = Array.append
    let cat_list = Array.concat
end

module Array_act = struct
    type 'a r = 'a array

    let length = Array.length
    let init n f = __builtin_effect (fun () -> Array.init n f)
    let init_fold n f accu =
	__builtin_effect (fun () -> Array_const.init_fold n f accu)
    let get i a = __builtin_effect (fun () -> Array.get a i)
    let slice n i a = __builtin_effect (fun () -> Array.sub a i n)
    let set i x a = __builtin_effect (fun () -> Array.set a i x)
    let copy a = __builtin_effect (fun () -> Array.copy a)
    let fill n i x a = __builtin_effect (fun () -> Array.fill a i n x)
    let blit n i a j b = __builtin_effect (fun () -> Array.blit a i b j n)
    let map f a = __builtin_effect (fun () -> Array.map f a)
    let mapi f a = __builtin_effect (fun () -> Array.mapi f a)
    let cat a b = __builtin_effect (fun () -> Array.append a b)
    let cat_list bs = __builtin_effect (fun () -> Array.concat bs)
    let sort cmp a =
	__builtin_effect (fun () -> Array.fast_sort (__adapt_cmp cmp) a)
    let of_list xs = __builtin_effect (fun () -> Array.of_list xs)
    let as_list a = __builtin_effect (fun () -> Array.to_list a)
    let of_array a = __builtin_effect (fun () -> Array.copy a)
    let as_array a = __builtin_effect (fun () -> Array.copy a)
end
