(* Copyright 2011  Petter Urkedal
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

(** Type Definitions of the Abstract Syntax Tree *)

TYPE_CONV_PATH "Camlviz.Ast_types"
open Sexplib.Conv
open Leaf_types
open Cst_types

let loc_of_sexp sx = Location.dummy
let sexp_of_loc loc = sexp_of_unit ()

type avar = Avar of loc * idr with sexp

type apath = Apath of avar list * avar with sexp

type atyp =
    | Atyp_ref of apath
    | Atyp_uvar of avar
    | Atyp_A of loc * avar * atyp
    | Atyp_E of loc * avar * atyp
    | Atyp_apply of loc * atyp * atyp
    | Atyp_arrow of loc * atyp * atyp
    with sexp

type apat =
    | Apat_literal of loc * lit
    | Apat_ref of apath
    | Apat_uvar of avar
    | Apat_apply of loc * apat * apat
    | Apat_as of loc * avar * apat
    | Apat_intype of loc * atyp * apat
    with sexp

type aval =
    | Aval_literal of loc * lit
    | Aval_ref of apath
    | Aval_apply of loc * aval * aval
    | Aval_array of loc * aval list
    | Aval_at of loc * (apat * aval option * aval) list
    | Aval_match of loc * aval * (apat * aval option * aval) list
    | Aval_let of loc * apat * aval * aval
    | Aval_letrec of loc * (loc * avar * atyp option * aval) list * aval
    | Aval_if of loc * aval * aval * aval
    | Aval_back of loc
    | Aval_seq of loc * idr * aval * aval option
    | Aval_raise of loc * aval
    with sexp

type ause = [`Stub_prefix of string | `type_c of avar * string]

type ainjnum = Ainjnum_auto | Ainjnum_cabi of string with sexp

type atypinfo =
    | Atypinfo_abstract (* only in signature *)
    | Atypinfo_alias of atyp
    | Atypinfo_injs of (loc * avar * atyp * ainjnum) list
    | Atypinfo_cabi of string
    with sexp

type asig =
    | Asig_ref of apath
    | Asig_decs of loc * adec list
    | Asig_product of loc * avar * asig * asig
    | Asig_suspension of loc * asig
    | Asig_with_type of loc * asig * atyp * atyp
    | Asig_with_struct of loc * asig * avar * apath
 and adec =
    | Adec_include of loc * asig
    | Adec_open of loc * apath
    | Adec_use of loc * aval
    | Adec_in of loc * avar * asig
    | Adec_sig of loc * avar * asig option
    | Adec_types of (loc * avar * atyp list * atypinfo) list
    | Adec_injx of loc * avar * atyp
    | Adec_val of loc * avar * atyp
    | Adec_cabi_val of loc * avar * atyp * string option * val_option list
    with sexp

type amod =
    | Amod_ref of apath
    | Amod_defs of loc * adef list
    | Amod_apply of loc * amod * amod
    | Amod_lambda of loc * avar * asig * amod
    | Amod_suspend of loc * amod
    | Amod_generate of loc * amod
    | Amod_coercion of loc * amod * asig
 and adef =
    | Adef_include of loc * amod
    | Adef_open of loc * apath
    | Adef_use of loc * aval
    | Adef_in of loc * avar * amod
    | Adef_sig of loc * avar * asig
    | Adef_types of (loc * avar * atyp list * atypinfo) list
    | Adef_injx of loc * avar * atyp
    | Adef_let of loc * apat * aval
    | Adef_letrec of (loc * avar * atyp option * aval) list
    | Adef_cabi_open of loc * string
    | Adef_cabi_val of loc * avar * atyp * string option * val_option list
    with sexp
