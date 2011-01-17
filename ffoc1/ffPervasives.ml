(* Copyright 2010  Petter Urkedal
 *
 * This file is part of Fform/OC <http://www.eideticdew.org/p/fform/>.
 *
 * Fform/OC is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fform/OC is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Fform/OC.  If not, see <http://www.gnu.org/licenses/>.
 *)

open CamomileLibrary.Default
include Camomile.UPervasives

exception Unimplemented

let ident x = x
let konst x y = x
let uncurry f (x, y) = f x y
let curry f x y = f (x, y)
let rec repeat n f x = if n = 0 then x else f (repeat (n - 1) f x)

let ( *< ) f g x = f (g x)
let ( *> ) f g x = g (f x)
let ( @< ) f x = f x

let int_of_digit ch =
    let i = Char.code ch in
    if 0x30 <= i && i <= 0x39 then i - 0x30 else
    raise (Failure "int_of_digit")


module List = struct
    include List

    let push x xs = x :: xs

    let rec last = function
	| [] -> invalid_arg "List.last"
	| [x] -> x
	| x :: xs -> last xs

    let rec fold f = function
	| [] -> fun accu -> accu
	| x :: xs -> fun accu -> fold f xs (f x accu)

    let rec find_image f = function
	| [] -> None
	| x :: xs -> match f x with Some y -> Some y
				  | None -> find_image f xs

    let split_where f xs =
	let rec loop ys = function
	    | [] -> ([], List.rev ys)
	    | x :: xs' as xs ->
		if f x then (xs, List.rev ys)
		else loop (x :: ys) xs'
	in loop [] xs

    let map_while f xs =
	let rec loop = function
	    | [], ys -> ([], List.rev ys)
	    | x :: xs as xs', ys ->
		begin match f x with
		| None -> (xs', List.rev ys)
		| Some y -> loop (xs, y :: ys)
		end
	in loop (xs, [])
end

module Char = struct
    include Char

    let is_space ch =
	match ch with ' ' | '\n' | '\t' -> true
		    | _ -> false
end

module String = struct
    include String

    let rindex_from s i ch =
	if i = String.length s then rindex s ch else
	rindex_from s i ch

    let split_on_char ch s =
	let rec split_before j accu =
	    if j <= 0 then accu else
	    let i = try rindex_from s j ch with Not_found -> 0 in
	    split_before (i - 1) (String.sub s i j :: accu) in
	split_before (String.length s) []
end
