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

let ( *< ) f g x = f (g x)
let ( *> ) f g x = g (f x)
let ( @< ) f x = f x

module List = struct
    include List

    let push x xs = x :: xs

    let rec fold f = function
	| [] -> fun accu -> accu
	| x :: xs -> fun accu -> fold f xs (f x accu)

    let rec find_image f = function
	| [] -> None
	| x :: xs -> match f x with Some y -> Some y
				  | None -> find_image f xs
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
