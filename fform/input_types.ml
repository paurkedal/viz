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
 * along with Fform/OC.  If not, see <http://www.gnu.org/licenses/>.
 *)

(* These type definitions are kept in a separate file without an interface file
 * to avoid duplicating all the constructor cases. *)

open Unicode

type loc = Location.t

type idr = Idr of string

type lit =
    | Lit_unit
    | Lit_int of int
    | Lit_float of float
    | Lit_string of UString.t

type trm =
    | Trm_ref		of loc * idr
    | Trm_literal	of loc * lit
    | Trm_label		of loc * idr * trm
    | Trm_lambda	of loc * trm * trm
    | Trm_quantify	of loc * idr * trm * trm
    | Trm_let		of loc * trm * trm * trm
    | Trm_rel		of loc * idr * trm * trm
    | Trm_rel_left	of loc * idr * trm * trm
    | Trm_apply		of loc * trm * trm
    | Trm_project	of loc * idr * trm
    | Trm_typing	of loc * trm * trm
    | Trm_raise		of loc * trm
    | Trm_if		of loc * trm * trm * trm
    | Trm_at		of loc * trm * trm * trm option
    | Trm_where		of loc * def list
    | Trm_with		of loc * trm option * def list
 and def =
    | Dec_type		of loc * trm
    | Def_type		of loc * trm * trm
    | Dec_struct	of loc * trm * trm
    | Def_struct	of loc * trm * trm
    | Dec_sig		of loc * idr
    | Def_sig		of loc * idr * trm
    | Dec_val		of loc * idr * trm
    | Def_val		of loc * trm * trm
    | Def_inj		of loc * idr * trm
    | Dec_lex		of loc * Opkind.t * idr list
