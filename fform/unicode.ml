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

open CamomileLibrary.Default.Camomile
open FfPervasives

module UChar = struct
    include UChar

    let ch_tab		= of_int 0x9
    let ch_nl		= of_int 0xa
    let ch_space	= of_int 0x20
    let ch_dash		= of_int 0x2d
    let ch_underscore	= of_int 0x5f

    let is_idrchr ch =
	match UCharInfo.general_category ch with
	| `Lu | `Ll | `Lt | `Mn | `Mc | `Me | `Nd | `Nl | `No | `Lm | `Lo -> true
	| `Pc -> ch == ch_underscore
	| _ -> false

    let is_space ch =
	match UCharInfo.general_category ch with
	| `Cc | `Zs | `Zl | `Zp (* | `Mc ? *) -> true
	| _ -> false

    let are_tied ch0 ch1 = is_idrchr ch0 && is_idrchr ch1
end

module UString = struct
    include UCS4
    let empty = init 0 (fun _ -> uchar_of_int 0)

    let of_list xs =
	let buf = Buf.create 8 in
	List.iter (Buf.add_char buf) xs;
	Buf.contents buf

    let of_sequence_n max_length xs =
	let buf = Buf.create 8 in
	Sequence.iter_n (Buf.add_char buf) max_length xs;
	Buf.contents buf

    module UString_encoding = CharEncoding.Make (UCS4)

    let of_utf8 = UString_encoding.decode CharEncoding.utf8
    let to_utf8 = UString_encoding.encode CharEncoding.utf8
end

module UString_sequence = struct
    type t = (UChar.t, UString.t * UString.index * UString.index) Sequence.t

    let create ws =
	let f (ws, ib, ie) =
	    if UString.compare_index ws ib ie > 0 then None else
	    Some (UString.look ws ib, (ws, (UString.next ws ib), ie)) in
	(f, (ws, UString.first ws, UString.last ws))

    let length (_, (ws, ib, ie)) = ie - ib + 1
    let position (_, (ws, ib, ie)) = ib
end

module UString_trie = struct
    include Trie.Make (UChar)

    let add_utf8 bs d =
	let ws = UString.of_utf8 bs in
	add (UString_sequence.create ws) d
end
