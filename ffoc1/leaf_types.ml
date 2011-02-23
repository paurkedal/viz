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

TYPE_CONV_PATH "Ffoc1.Cst_types"
open Sexplib
open Unicode

type idr = Idr of string with sexp

type lit =
    | Lit_unit
    | Lit_bool of bool
    | Lit_int of int
    | Lit_float of float
    | Lit_string of UString.t
    with sexp

type stratum = [`Type | `Value | `Signature | `Structure]

type abi = Abi_Fform | Abi_C with sexp

type type_exposure = [`Default | `Local | `Abstract | `Exported]
type val_exposure = [`Default | `Local | `Exported]

type val_info = val_exposure * abi
