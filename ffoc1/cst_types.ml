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

(** Type Definitions of the Concrete Syntax Tree *)

(* These type definitions are kept in a separate file without an interface file
 * to avoid duplicating all the constructor cases. *)

TYPE_CONV_PATH "Ffoc1.Cst_types"
open Sexplib
open Leaf_types

type loc = Location.t

type idrhint = Ih_none | Ih_univ | Ih_inj

type cidr = Cidr of loc * idr

type cmonad = string

type cpred =
    | Cpred_let		of loc * cmonad option * ctrm * cpred * cpred
    | Cpred_if		of loc * ctrm * cpred * cpred
    | Cpred_at		of loc * (ctrm * cpred) list
    | Cpred_be		of loc * ctrm
    | Cpred_assert	of loc * ctrm * cpred
    | Cpred_raise	of loc * ctrm
    | Cpred_do1		of loc * cmonad * ctrm
    | Cpred_do2		of loc * cmonad * ctrm * cpred
 and ctrm =
    | Ctrm_ref		of cidr * idrhint
    | Ctrm_literal	of loc * lit
    | Ctrm_label	of loc * cidr * ctrm
    | Ctrm_quantify	of loc * cidr * ctrm * ctrm
    | Ctrm_rel		of loc * ctrm * (loc * cidr * ctrm) list
    | Ctrm_apply	of loc * ctrm * ctrm
    | Ctrm_project	of loc * cidr * ctrm
    | Ctrm_array	of loc * ctrm list
    | Ctrm_what		of loc * cmonad option * cpred
    | Ctrm_where	of loc * cdef list
    | Ctrm_with		of loc * ctrm option * cdef list
 and cdef =
    | Cdef_include	of loc * ctrm
    | Cdef_open		of loc * abi * ctrm
    | Cdef_type		of loc * ctrm
    | Cdef_in		of loc * ctrm * ctrm
    | Cdec_sig		of loc * cidr
    | Cdef_sig		of loc * cidr * ctrm
    | Cdef_val		of loc * val_info * ctrm
    | Cdef_let		of loc * cmonad option * ctrm * cpred
    | Cdef_inj		of loc * ctrm
    | Cdef_lex		of loc * string * (cidr * cidr list) list
