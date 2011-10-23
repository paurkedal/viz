(* Copyright 2010--2011  Petter Urkedal
 *
 * This file is part of the Viz Compiler <http://www.vizlang.org/>.
 *
 * The Viz Compiler is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * The Viz Compiler is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with the Viz Compiler.  If not, see <http://www.gnu.org/licenses/>.
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
let ( |> ) x f = f x

let int_of_digit ch =
    let i = Char.code ch in
    if 0x30 <= i && i <= 0x39 then i - 0x30 else
    raise (Failure "int_of_digit")

module Bool = struct
    let power b f x = if b then f x else x
end

module List = struct
    include List

    let push x xs = x :: xs

    let rec last = function
	| [] -> invalid_arg "List.last"
	| [x] -> x
	| x :: xs -> last xs

    let rec compare_with cmp xs ys =
	match xs, ys with
	| [], [] -> 0
	| [], _ -> -1
	| _, [] ->  1
	| x :: xs', y :: ys' ->
	    let c = cmp x y in
	    if c <> 0 then c else compare_with cmp xs' ys'

    let init i f =
	let rec loop i accu =
	    if i < 0 then accu
	    else loop (i - 1) (f i :: accu)
	in loop (i - 1) []

    let rec fold f = function
	| [] -> fun accu -> accu
	| x :: xs -> fun accu -> fold f xs (f x accu)

    let map_fold f (xs, accu) =
	let rec loop ys accu = function
	    | [] -> (List.rev ys, accu)
	    | x :: xs ->
		let (y, accu') = f (x, accu) in
		loop (y :: ys) accu' xs in
	loop [] accu xs

    let combine f = function
	| [] -> raise (Failure "reduce on empty list")
	| [x] -> x
	| x :: xs -> fold f xs x

    let rev_filter f =
	let rec loop accu = function
	    | [] -> accu
	    | x :: xs -> if f x then loop (x :: accu) xs else loop accu xs
	in loop []

    let rec find_image f = function
	| [] -> None
	| x :: xs -> match f x with Some y -> Some y
				  | None -> find_image f xs

    let rec split_before f =
	let rec loop ys = function
	    | [] -> raise Not_found
	    | x :: xs ->
		if f x then (List.rev ys, x :: xs)
		else loop (x :: ys) xs
	in loop []

    let rec split_after f =
	let rec loop ys = function
	    | [] -> raise Not_found
	    | x :: xs ->
		if f x then (List.rev (x :: ys), xs)
		else loop (x :: ys) xs
	in loop []

    let map_while f xs =
	let rec loop = function
	    | [], ys -> ([], List.rev ys)
	    | x :: xs as xs', ys ->
		begin match f x with
		| None -> (xs', List.rev ys)
		| Some y -> loop (xs, y :: ys)
		end
	in loop (xs, [])

    let rec drop_while f = function
	| x :: xs -> if f x then drop_while f xs else x :: xs
	| [] -> []

    let even_odd_pairs zs =
	let rec loop accu = function
	    | x :: y :: zs -> loop ((x, y) :: accu) zs
	    | x :: zs -> (rev accu, Some x)
	    | [] -> (rev accu, None) in
	loop [] zs
end

module Char = struct
    include Char

    let is_space ch =
	match ch with ' ' | '\n' | '\t' -> true
		    | _ -> false

    let is_lower ch = let code = Char.code ch in 0x41 <= code && code <= 0x5a
    let is_upper ch = let code = Char.code ch in 0x61 <= code && code <= 0x7a
end

module String = struct
    include String

    let rec skip_while f s i =
	if i < length s && f s.[i] then skip_while f s (i + 1) else i

    let map_of_list f xs =
	let buf = Buffer.create 8 in
	List.iter (Buffer.add_char buf *< f) xs;
	Buffer.contents buf

    let rindex_from s i ch =
	if i = String.length s then rindex s ch else
	rindex_from s i ch

    let split_on_char ch s =
	let rec split_before j accu =
	    if j <= 0 then accu else
	    let i = try rindex_from s (j - 1) ch + 1 with Not_found -> 0 in
	    split_before (i - 1) (String.sub s i (j - i) :: accu) in
	split_before (String.length s) []

    let after i s = sub s i (String.length s - i)

    let starts_with pfx s =
	let n = String.length s in
	let m = String.length pfx in
	m <= n && String.sub s 0 m = pfx

    let ends_with sfx s =
	let n = String.length s in
	let m = String.length sfx in
	m <= n && String.sub s (n - m) m = sfx

    let strip_suffix sfx s =
	let n = String.length s in
	let m = String.length sfx in
	if m <= n && String.sub s (n - m) m = sfx
	then String.sub s 0 (n - m)
	else s

    let join sep = function
	| [] -> ""
	| x :: xs ->
	    let buf = Buffer.create 32 in
	    Buffer.add_string buf x;
	    List.iter
		begin fun x ->
		    Buffer.add_string buf sep;
		    Buffer.add_string buf x
		end xs;
	    Buffer.contents buf
end
