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

open FfPervasives

type ('a, 'b) t = ('b -> ('a * 'b) option) * 'b

let pop (g, xs) =
    match g xs with
    | None -> None
    | Some (x, xs') -> Some (x, (g, xs'))

let peek (g, xs) =
    match g xs with
    | None -> None
    | Some (x, _) -> Some x

let rec fold f (g, xs) accu =
    match g xs with
    | None -> accu
    | Some (x, xs') -> fold f (g, xs') (f x accu)

let rec iter f (g, xs) =
    match g xs with
    | None -> ()
    | Some (x, xs') -> f x; iter f (g, xs')

let rec iter_n f n (g, xs) =
    if n <= 0 then () else
    match g xs with
    | None -> ()
    | Some (x, xs') -> f x; iter_n f (n - 1) (g, xs')

let of_list xs =
    let g = function [] -> None | x :: xs -> Some (x, xs) in
    (g, xs)

let to_list xseq = List.rev (fold List.push xseq [])
