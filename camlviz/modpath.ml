(* Copyright 2011  Petter Urkedal
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

open FfPervasives
open Leaf_types
open Leaf_core
open Sexplib

type t = idr list

let valid_idr (Idr s) =
    not (String.contains s '.')
    || Char.is_digit s.[0] && s.[1] = '\''

let empty = []

let is_empty = function [] -> true | _ -> false

let atom x = assert (valid_idr x); [x]

let is_atom = function [_] -> true | _ -> false

let cat_last x xs = assert (valid_idr x); x :: xs

let cut_last = function
    | x :: xs -> Some (x, xs)
    | [] -> None

let last_e = function
    | x :: xs -> x
    | [] -> failwith "Modpath.last_e"

let strip_last_e = function
    | x :: xs -> xs
    | [] -> failwith "Modpath.strip_last_e"

let length = List.length

let rec nth_last n =
    function
    | [] -> None
    | x :: xs ->
	if n = 0 then Some x else
	nth_last (n - 1) xs

let rec has_suffix zs xs =
    match zs, xs with
    | [], _ -> true
    | _ :: _, [] -> false
    | z :: zs', x :: xs' -> z = x && has_suffix zs' xs'

let has_prefix zs xs = has_suffix (List.rev zs) (List.rev xs)

let cat xs ys = ys @ xs

let rec strip_suffix zs xs =
    match zs, xs with
    | [], _ -> Some xs
    | _ :: _, [] -> None
    | z :: zs', x :: xs' -> if z = x then strip_suffix zs' xs' else None

let strip_prefix zs xs =
    Option.map List.rev (strip_suffix (List.rev zs) (List.rev xs))

let strip_suffix_e zs xs = Option.get (strip_suffix zs xs)
let strip_prefix_e zs xs = Option.get (strip_prefix zs xs)

let rec strip_common_suffix zs xs =
    match zs, xs with
    | [], _ -> xs
    | _ :: _, [] -> []
    | z :: zs', x :: xs' -> if z = x then strip_common_suffix zs' xs' else xs
let strip_common_prefix zs xs =
    List.rev (strip_common_suffix (List.rev zs) (List.rev xs))

let fold = List.fold_right

let rfold = List.fold

let rec iter f = function
    | [] -> ()
    | x :: xs -> iter f xs; f x

let riter = List.iter

let compare xs ys =
    match xs, ys with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | x :: xs', y :: ys' ->
	let c = compare x y in
	if c <> 0 then c else
	compare xs' ys'

let to_string xs = String.join "." (List.rev_map idr_to_string xs)
let of_string s = List.rev_map idr_of_string (String.split_on_char '.' s)
let to_idr_list = List.rev
let of_idr_list = List.rev
let to_string_list = List.rev_map idr_to_string
let of_string_list = List.rev_map idr_of_string

let t_of_sexp sx = of_string (Sexp.to_string sx)
let sexp_of_t p = (Sexp.of_string (to_string p))

module Set = Set.Make (struct type t = idr list let compare = compare end)
