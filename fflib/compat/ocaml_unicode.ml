(* Copyright 2011  Petter Urkedal
 *
 * This file is part of the Fform Standard Library.
 *
 * The Fform Standard Library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * Fform is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Fform.  If not, see <http://www.gnu.org/licenses/>.
 *)

open CamomileLibrary.Default.Camomile

module UString_encoding = CharEncoding.Make (UText)

type char = UChar.t
type utf8 = string
type string = UText.t

module Char = struct
    type t = char
    let of_int = UChar.chr
    let as_int = UChar.code
end

module String = struct
    type t = string
    let of_utf8 = UString_encoding.decode CharEncoding.utf8
    let as_utf8 = UString_encoding.encode CharEncoding.utf8
end
