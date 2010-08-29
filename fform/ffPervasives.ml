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
 * along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
 *)

open CamomileLibrary.Default
include Camomile.UPervasives

exception Unimplemented

let ident x = x
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
