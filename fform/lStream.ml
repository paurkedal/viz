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

open Unicode
open CamomileLibrary.Default.Camomile

type elt = UChar.t
type t = {
    stream : UChar.t Stream.t;
    mutable locb : Location.Bound.t;
}

let open_in path =
    let utf8_stream = Stream.of_channel (open_in path) in
    let stm = CharEncoding.ustream_of CharEncoding.utf8 utf8_stream in
    {stream = stm; locb = Location.Bound.init path}

let locbound stm = stm.locb

let pop stm =
    try
	let ch = Stream.next stm.stream in
	stm.locb <- Location.Bound.skip_char ch stm.locb;
	Some ch
    with Stream.Failure -> None

let peek stm = Stream.peek stm.stream

let peek_n n stm = UString.of_list (Stream.npeek n stm.stream)

let peek_at i stm =
    let rec f i = function
	| [] -> None
	| x :: xs -> if i = 0 then Some x else f (i - 1) xs
    in f i (Stream.npeek (i + 1) stm.stream)

let skip stm =
    let ch = Stream.next stm.stream in
    stm.locb <- Location.Bound.skip_char ch stm.locb

let rec skip_n n stm =
    if n = 0 then () else
    begin
	skip stm;
	skip_n (n - 1) stm
    end
