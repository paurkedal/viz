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

include Input_types

let idr_of_string name = Idr name
let idr_to_string (Idr name) = name
let idr_of_ustring name = Idr (Unicode.UString.to_utf8 name)
let idr_to_ustring (Idr name) = Unicode.UString.of_utf8 name
let idr_1o (Idr name) = Idr ("1o" ^ name)
let idr_2o (Idr name) = Idr ("2o" ^ name)

let trm_ref loc name = Trm_ref (loc, idr_of_string name)

let tuple_op = trm_ref Location.dummy "2o,"
