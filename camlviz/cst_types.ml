(* Copyright 2010--2011  Petter Urkedal
 *
 * This file is part of the Viz Compiler <http://www.vizlang.org/>.
 *
 * The Viz Compiler is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * The Viz Compiler is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with the Viz Compiler.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Type Definitions of the Concrete Syntax Tree *)

(* These type definitions are kept in a separate file without an interface file
 * to avoid duplicating all the constructor cases. *)

TYPE_CONV_PATH "Camlviz.Cst_types"
open Sexplib
open Leaf_types

type idrhint = Ih_none | Ih_univ | Ih_inj

type cidr = Cidr of loc * idr

type cmonad = string

type cpred =
    | Cpred_let		of loc * cmonad option * ctrm * cpred * cpred
    | Cpred_if		of loc * ctrm * cpred * cpred
    | Cpred_back	of loc
    | Cpred_at		of loc * (ctrm * cpred) list
    | Cpred_be		of loc * ctrm
    | Cpred_seq		of loc * idr * ctrm * cpred option
    | Cpred_seq_which	of loc * idr * ctrm * cwhich * cpred option
    | Cpred_iterate	of loc * idr * ctrm * cpred * cpred option
    | Cpred_raise	of loc * ctrm
    | Cpred_upon	of loc * ctrm * cpred * cpred
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
    | Cdef_include	of loc * bool * ctrm
    | Cdef_open		of loc * abi * ctrm
    | Cdef_use		of loc * ctrm
    | Cdef_type		of loc * abi * ctrm * cdef list
    | Cdef_in		of loc * bool * ctrm * ctrm
    | Cdec_sig		of loc * cidr
    | Cdef_sig		of loc * cidr * ctrm
    | Cdef_val		of loc * val_info * ctrm
    | Cdef_let		of loc * cmonad option * ctrm * cpred
    | Cdef_inj		of loc * abi * ctrm
    | Cdef_lex		of loc * string * (cidr * cidr list) list
    | Cdef_lexalias	of loc * (cidr * cidr) list
 and cwhich = cmonad option * cpred
