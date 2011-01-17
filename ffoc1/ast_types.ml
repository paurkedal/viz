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

open Cst_types

type avar = Avar of loc * idr

type apath = Apath of avar list * avar

type atyp =
    | Atyp_ref of apath
    | Atyp_uvar of avar
    | Atyp_apply of loc * atyp * atyp
    | Atyp_arrow of loc * atyp * atyp

type apat =
    | Apat_ref of apath
    | Apat_uvar of avar
    | Apat_apply of loc * apat * apat

type aval =
    | Aval_literal of loc * lit
    | Aval_ref of apath
    | Aval_apply of loc * aval * aval
    | Aval_at of loc * (apat * aval option * aval) list
    | Aval_match of loc * aval * (apat * aval option * aval) list
    | Aval_let of loc * (loc * apat * aval) list * aval
    | Aval_if of loc * aval * aval * aval
    | Aval_raise of loc * aval

type atypinfo =
    | Atypinfo_alias of atyp
    | Atypinfo_injs of (loc * avar * atyp) list

type asig =
    | Asig_ref of apath
    | Asig_decs of loc * adec list
    | Asig_product of loc * avar * asig * asig
    | Asig_with_type of loc * avar * atyp
    | Asig_with_sig of loc * avar * asig
 and adec =
    | Adec_include of loc * asig
    | Adec_open of loc * apath
    | Adec_in of loc * avar * asig
    | Adec_sig of loc * avar * asig option
    | Adec_types of (loc * avar * atyp list * atypinfo) list
      (** Holds a non-empty list of mutually recursive type definitions. *)
    | Adec_val of loc * avar * atyp

type amod =
    | Amod_ref of apath
    | Amod_defs of loc * adef list
    | Amod_apply of loc * amod * amod
    | Amod_lambda of loc * avar * asig * amod
    | Amod_coercion of loc * amod * asig
 and adef =
    | Adef_include of loc * amod
    | Adef_open of loc * apath
    | Adef_in of loc * avar * amod
    | Adef_sig of loc * avar * asig
    | Adef_types of (loc * avar * atyp list * atypinfo) list
    | Adef_vals of (loc * avar * aval) list