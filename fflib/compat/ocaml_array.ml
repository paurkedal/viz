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

open Ocaml_prereq

let fold f a accu = Array.fold_left (fun accu x -> f x accu) accu a

module Array_const = struct
    type 'a t = 'a array

    let length = Array.length
    let get i a = Array.get a i
    let slice n i a = Array.sub a i n
    let init n f = Array.init n f
    let uniform n x = Array.make n x
    let of_list = Array.of_list
    let as_list = Array.to_list
    let map = Array.map
    let mapi = Array.mapi
    let fold = fold
    let foldr = Array.fold_right
    let cat = Array.append
    let cat_list = Array.concat
end

module Array_act = struct
    type 'a r = 'a array

    let init n f = __unsafe_action (fun () -> Array.init n f)
    let uniform n x = __unsafe_action (fun () -> Array.make n x)
    let get i a = __unsafe_action (fun () -> Array.get a i)
    let slice n i a = __unsafe_action (fun () -> Array.sub a i n)
    let set i x a = __unsafe_action (fun () -> Array.set a i x)
    let copy a = __unsafe_action (fun () -> Array.copy a)
    let fill n i x a = __unsafe_action (fun () -> Array.fill a i n x)
    let blit n i a j b = __unsafe_action (fun () -> Array.blit a i b j n)
    let map f a = __unsafe_action (fun () -> Array.map f a)
    let mapi f a = __unsafe_action (fun () -> Array.mapi f a)
    let fold f a accu = __unsafe_action (fun () -> fold f a accu)
    let foldr f a accu = __unsafe_action (fun () -> Array.fold_right f a accu)
    let cat a b = __unsafe_action (fun () -> Array.append a b)
    let cat_list bs = __unsafe_action (fun () -> Array.concat bs)
    let sort cmp a =
	__unsafe_action (fun () -> Array.fast_sort (__adapt_cmp cmp) a)
    let of_list xs = __unsafe_action (fun () -> Array.of_list xs)
    let as_list a = __unsafe_action (fun () -> Array.to_list a)
    let of_array a = __unsafe_action (fun () -> Array.copy a)
    let as_array a = __unsafe_action (fun () -> Array.copy a)
end