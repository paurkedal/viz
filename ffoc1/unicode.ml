(* Copyright 2010--2011  Petter Urkedal
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

open CamomileLibrary.Default.Camomile
open FfPervasives
open Sexplib

module UString_encoding = CharEncoding.Make (UText)

module UChar = struct
    include UChar

    let ch_tab		= of_int 0x9
    let ch_nl		= of_int 0xa
    let ch_space	= of_int 0x20
    let ch_hash		= of_int 0x23
    let ch_apostrophe	= of_int 0x27
    let ch_comma	= of_int 0x2c
    let ch_dash		= of_int 0x2d
    let ch_dot		= of_int 0x2e
    let ch_qmark	= of_int 0x3f
    let ch_underscore	= of_int 0x5f
    let ch_grave_accent	= of_int 0x60
    let ch_lbrace	= of_int 0x7b
    let ch_rbrace	= of_int 0x7d

    let is_idrchr ch =
	match UCharInfo.general_category ch with
	| `Lu | `Ll | `Lt | `Mn | `Mc | `Me | `Nd | `Nl | `No | `Lm | `Lo -> true
	| `Pc -> ch == ch_underscore
	| `Po -> ch == ch_apostrophe
	| `Sk -> ch == ch_grave_accent
	| _ -> false

    let is_space ch =
	match UCharInfo.general_category ch with
	| `Cc | `Zs | `Zl | `Zp (* | `Mc ? *) -> true
	| _ -> false

    let is_hspace ch =
	match UCharInfo.general_category ch with
	| `Cc ->
	    begin match int_of_uchar ch with
	    | 0x9 -> true
	    | _ -> false
	    end
	| `Zs -> true
	| _ -> false

    let is_ascii_digit ch = let i = uint_code ch in 0x30 <= i && i <= 0x39
    let is_ascii_lower ch = let i = uint_code ch in 0x61 <= i && i <= 0x7a
    let is_ascii_upper ch = let i = uint_code ch in 0x41 <= i && i <= 0x5a
    let is_ascii_alpha ch = is_ascii_lower ch || is_ascii_upper ch
    let is_ascii_alnum ch = is_ascii_alpha ch || is_ascii_digit ch

    let is_greek_lower ch =
	UCharInfo.general_category ch == `Ll && UCharInfo.script ch == `Greek
    let is_greek_upper ch =
	UCharInfo.general_category ch == `Lu && UCharInfo.script ch == `Greek
    let is_greek_alpha ch =
	match UCharInfo.general_category ch with
	| `Ll | `Lu -> UCharInfo.script ch == `Greek
	| _ -> false

    let is_ocaml_idrfst ch = is_ascii_alpha ch || uint_code ch == 0x5f
    let is_ocaml_idrcnt ch =
	is_ascii_alnum ch ||
	match uint_code ch with 0x27 | 0x5f -> true | _ -> false

    let are_tied ch0 ch1 =
	   is_idrchr ch0 && is_idrchr ch1
	|| ch0 = ch_dash && is_ascii_digit ch1

    let to_utf8 ch =
	UString_encoding.encode CharEncoding.utf8 (UText.init 1 (fun _ -> ch))
end

module UChar_map = Map.Make (UChar)

module UString = struct
    include UText

    let empty = init 0 (fun _ -> uchar_of_int 0)

    let after i s = sub s i (length s - i)

    let of_list xs =
	let buf = Buf.create 8 in
	List.iter (Buf.add_char buf) xs;
	Buf.contents buf

    let of_sequence_n max_length xs =
	let buf = Buf.create 8 in
	Sequence.iter_n (Buf.add_char buf) max_length xs;
	Buf.contents buf

    let of_utf8 = UString_encoding.decode CharEncoding.utf8
    let to_utf8 = UString_encoding.encode CharEncoding.utf8

    let t_of_sexp sx = of_utf8 (Conv.string_of_sexp sx)
    let sexp_of_t s = (Conv.sexp_of_string (to_utf8 s))
end

module UString_sequence = struct
    type t = (UChar.t, UString.t * UString.index * UString.index) Sequence.t

    let create ws =
	let f (ws, ib, ie) =
	    if UString.compare_index ws ib ie > 0 then None else
	    Some (UString.look ws ib, (ws, (UString.next ws ib), ie)) in
	(f, (ws, UString.first ws, UString.last ws))
end

module UString_trie = struct
    include Trie.Make (UChar)

    let add_utf8 bs d =
	let ws = UString.of_utf8 bs in
	add (UString_sequence.create ws) d
end
