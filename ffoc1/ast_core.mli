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

open Ast_types
open Cst_types

val avar_idr : avar -> idr

val avar_loc : avar -> loc
val apath_loc : apath -> loc
val atyp_loc : atyp -> loc
val aval_loc : aval -> loc
val apat_loc : apat -> loc
val asig_loc : asig -> loc
val adec_loc : adec -> loc
val amod_loc : amod -> loc
val adef_loc : adef -> loc