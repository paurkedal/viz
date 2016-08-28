(* Copyright (C) 2011--2016  Petter A. Urkedal <paurkedal@gmail.com>
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
open CamomileLibraryDefault.Camomile

module Pervasive : sig
  type char = UChar.t
  type utf8_string = string
  type string = UText.t

  val __char_of_code : int -> char
  val __char_code : char -> int
  val __string_of_utf8 : utf8_string -> string
end
open Pervasive

module Char_ : sig
  val of_int : int -> char
  val as_int : char -> int
end

module String_ : sig
  val length : string -> int
  val get : int -> string -> char
  val init : int -> (int -> char) -> string

  val eq : string -> string -> bool
  val cmp : string -> string -> torder

  val of_utf8 : utf8_string -> string
  val as_utf8 : string -> utf8_string
end

module String_buf : sig
  type 'f r

  val create : ('f, 'f r) effect
  val contents : 'f r -> ('f, string) effect
  val length : 'f r -> ('f, int) effect
  val clear : 'f r -> ('f, unit) effect
  val put_char : 'f r -> char -> ('f, unit) effect
  val put_string : 'f r -> string -> ('f, unit) effect
end

module Utf8_string : sig
  val of_string : string -> utf8_string
  val as_string : utf8_string -> string
  val length : utf8_string -> int
end
