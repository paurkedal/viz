(* Copyright 2011  Petter Urkedal
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
open Leaf_types
open FfPervasives

let lit_to_string = function
    | Lit_unit -> "()"
    | Lit_bool x -> if x then "true" else "false"
    | Lit_int i -> string_of_int i
    | Lit_float x -> string_of_float x
    | Lit_char s -> "c\"" ^ String.escaped (UChar.to_utf8 s) ^ "\""
    | Lit_string s -> "\"" ^ String.escaped (UString.to_utf8 s) ^ "\""

let starts_with = String.starts_with

let idr_of_string name = Idr name
let idr_to_string (Idr name) = name
let idr_of_ustring name = Idr (UString.to_utf8 name)
let idr_to_ustring (Idr name) = UString.of_utf8 name

let idr_0o_c name = Idr ("0'" ^ name)
let idr_0o (Idr name) = idr_0o_c name
let idr_0o_symbol (Idr name) =
    if starts_with "0'" name then String.sub name 2 (String.length name - 2)
    else raise (Failure ("Expected a unary operator identifier: " ^ name))

let idr_1o_c name = Idr ("1'" ^ name)
let idr_1o (Idr name) = idr_1o_c name
let idr_1o_symbol (Idr name) =
    if starts_with "1'" name then String.sub name 2 (String.length name - 2)
    else raise (Failure ("Expected a unary operator identifier: " ^ name))

let idr_2o_c name = Idr ("2'" ^ name)
let idr_2o (Idr name) = idr_2o_c name
let idr_2o_symbol (Idr name) =
    if starts_with "2'" name then String.sub name 2 (String.length name - 2)
    else raise (Failure ("Expected a binary operator identifier: " ^ name))

let idr_0b_c lname rname = Idr ("0'" ^ lname ^ "'" ^ rname)
let idr_0b (Idr lname) (Idr rname) = idr_0b_c lname rname
let idr_1b_c lname rname = Idr ("1'" ^ lname ^ "'" ^ rname)
let idr_1b (Idr lname) (Idr rname) = idr_1b_c lname rname
let idr_2b_c lname rname = Idr ("2'" ^ lname ^ "'" ^ rname)
let idr_2b (Idr lname) (Idr rname) = idr_2b_c lname rname

let idr_1q_c name = Idr ("1'" ^ name)
let idr_1q (Idr name) = idr_1q_c name
let idr_1q_symbol (Idr name) =
    if starts_with "1'" name then String.sub name 2 (String.length name - 2)
    else raise (Failure ("Expected a quantifier operator identifier: " ^ name))

module String_map = Map.Make (String)
module Idr = struct
    type t = idr
    let compare (Idr x) (Idr y) = compare x y
end
module Idr_set = Set.Make (Idr)
module Idr_map = Map.Make (Idr)
