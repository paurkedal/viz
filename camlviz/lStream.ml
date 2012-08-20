(* Copyright 2010--2012  Petter Urkedal
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

open Unicode
open CamomileLibraryDefault.Camomile

type elt = UChar.t
type t = {
    stream : UChar.t Stream.t;
    mutable locb : Location.Bound.t;
}

let null = {stream = Stream.of_list []; locb = Location.Bound.dummy}

let of_string ?(locb = Location.Bound.dummy) s =
    let utf8_stream = Stream.of_string s in
    let stm = CharEncoding.ustream_of CharEncoding.utf8 utf8_stream in
    {stream = stm; locb = locb}

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

let pop_e stm =
    match pop stm with
    | Some ch -> ch
    | None -> raise Stream.Failure

let peek stm = Stream.peek stm.stream

let peek_e stm =
    match Stream.peek stm.stream with
    | Some ch -> ch
    | None -> raise Stream.Failure

let peek_n n stm = Stream.npeek n stm.stream

let peek_at i stm =
    let rec f i = function
	| [] -> None
	| x :: xs -> if i = 0 then Some x else f (i - 1) xs
    in f i (Stream.npeek (i + 1) stm.stream)

let pop_code stm =
    match pop stm with
    | None -> 0
    | Some ch -> UChar.code ch

let peek_code stm =
    match Stream.peek stm.stream with
    | None -> 0
    | Some ch -> UChar.code ch

let peek_n_code n stm =
    List.map UChar.code (Stream.npeek n stm.stream)

let skip stm =
    let ch = Stream.next stm.stream in
    stm.locb <- Location.Bound.skip_char ch stm.locb

let rec skip_n n stm =
    if n = 0 then () else
    begin
	skip stm;
	skip_n (n - 1) stm
    end

let skip_while f stm =
    while Option.exists f (peek stm) do skip stm done

let scan_while f stm =
    let buf = UString.Buf.create 8 in
    let loc_lb = locbound stm in
    while
	match peek stm with
	| None -> false
	| Some ch -> f ch && (UString.Buf.add_char buf ch; true)
    do skip stm done;
    let loc_ub = locbound stm in
    (UString.Buf.contents buf, Location.between loc_lb loc_ub)
