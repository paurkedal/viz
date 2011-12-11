(* Copyright 2011  Petter Urkedal
 *
 * This file is part of the Viz Standard Library <http://www.vizlang.org/>.
 *
 * The Viz Standard Library (VSL) is free software: you can redistribute it
 * and/or modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * The VSL is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the VSL.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ocaml_prereq
open CamomileLibrary.Default.Camomile

module UString_encoding = CharEncoding.Make (UText)

module Pervasive = struct
    type char = UChar.t
    type utf8_string = string
    type string = UText.t

    let __string_of_utf8 = UString_encoding.decode CharEncoding.utf8
    let __char_of_code i = UChar.chr i
    let __char_code ch = UChar.code ch
end
open Pervasive

module Char_ = struct
    let of_int = UChar.chr
    let as_int = UChar.code
end

module String_ = struct
    let length = UText.length
    let get i s = UText.get s i
    let init = UText.init

    let of_utf8 = UString_encoding.decode CharEncoding.utf8
    let as_utf8 = UString_encoding.encode CharEncoding.utf8

    let eq (x : string) (y : string) = x = y
    let cmp (x : string) (y : string) = __generic_cmp x y
end

module String_buf = struct
    module B = UText.Buf
    type 'f r = B.buf

    let create = __builtin_effect (fun () -> B.create 8)
    let contents buf = __builtin_effect (fun () -> B.contents buf)
    let length buf = __builtin_effect (fun () -> B.length buf)
    let clear buf = __builtin_effect (fun () -> B.clear buf)
    let put_char buf ch = __builtin_effect (fun () -> B.add_char buf ch)
    let put_string buf s = __builtin_effect (fun () -> B.add_string buf s)
end

module Utf8_string = struct
    let of_string = UString_encoding.encode CharEncoding.utf8
    let as_string = UString_encoding.decode CharEncoding.utf8

    let length = String.length
end

let () =
    Callback.register "ustring_of_utf8" Utf8_string.as_string;
    Callback.register "ustring_to_utf8" Utf8_string.of_string;
