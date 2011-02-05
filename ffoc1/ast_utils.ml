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

open Ast_core
open Ast_types
open Diag
open Sexplib
open FfPervasives

let sexp_to_string sx =
    let buf = Buffer.create 64 in
    Sexp.to_buffer_hum buf sx;
    Buffer.contents buf
let atyp_to_string x = sexp_to_string (sexp_of_atyp x)
let aval_to_string x = sexp_to_string (sexp_of_aval x)
let asig_to_string x = sexp_to_string (sexp_of_asig x)
let amod_to_string x = sexp_to_string (sexp_of_amod x)

let apath_to_avar = function
    | Apath ([], v) -> v
    | p -> errf_at (apath_loc p) "Expecting an unqualified identifier."

let rec result_type = function
    | Atyp_arrow (_, _, rt) -> result_type rt
    | rt -> rt

let rec fold_arg_types f = function
    | Atyp_arrow (_, at, rt) -> f at *> fold_arg_types f rt
    | rt -> ident

let flatten_arrows =
    let rec loop ats = function
	| Atyp_arrow (_, at, rt) -> loop (at :: ats) rt
	| rt -> (rt, List.rev ats) in
    loop []

let flatten_application =
    let rec loop args = function
	| Atyp_apply (_, at, arg) -> loop (arg :: args) at
	| Atyp_ref p -> (p, List.rev args)
	| at -> errf_at (atyp_loc at) "Expecting a type constructor." in
    loop []


(* Folds over Path Subterms *)

let rec fold_atyp_paths f = function
    | Atyp_ref p -> f p
    | Atyp_uvar _ -> ident
    | Atyp_apply (_, u, v) | Atyp_arrow (_, u, v) ->
	fold_atyp_paths f u *> fold_atyp_paths f v

let rec fold_apat_paths f = function
    | Apat_literal _ -> ident
    | Apat_ref p -> f p
    | Apat_uvar _ -> ident
    | Apat_apply (_, p, q) ->
	fold_apat_paths f p *> fold_apat_paths f q

let rec fold_aval_paths f =
    let fold_cases =
	List.fold
	    begin fun (p, cond, cq) ->
		fold_apat_paths f p *>
		Option.fold (fold_aval_paths f) cond *>
		fold_aval_paths f cq
	    end in
    function
    | Aval_literal _ -> ident
    | Aval_ref p -> f p
    | Aval_apply (_, x, y) -> fold_aval_paths f x *> fold_aval_paths f y
    | Aval_at (_, cases) -> fold_cases cases
    | Aval_match (_, x, cases) -> fold_aval_paths f x *> fold_cases cases
    | Aval_let (_, bindings, body) ->
	List.fold
	    (fun (_, p, x) -> fold_apat_paths f p *> fold_aval_paths f x)
	    bindings *>
	fold_aval_paths f body
    | Aval_if (_, c, cq, ccq) ->
	fold_aval_paths f c *> fold_aval_paths f cq *> fold_aval_paths f ccq
    | Aval_raise (_, x) -> fold_aval_paths f x

let fold_atypinfo_paths f = function
    | Atypinfo_abstract -> ident
    | Atypinfo_alias u -> fold_atyp_paths f u
    | Atypinfo_injs injs ->
	List.fold (fun (_, _, u) -> fold_atyp_paths f u) injs

let fold_atypbind_paths f (_, _, us, ti) =
    List.fold (fold_atyp_paths (f `Type)) us *>
    fold_atypinfo_paths (f `Type) ti

let rec fold_asig_paths f = function
    | Asig_ref p -> f `Signature p
    | Asig_decs (_, decs) -> List.fold (fold_adec_paths f) decs
    | Asig_product (_, _, r, s) -> fold_asig_paths f r *> fold_asig_paths f s
    | Asig_with_type (_, s, _, u) ->
	fold_asig_paths f s *> fold_atyp_paths (f `Type) u
    | Asig_with_struct (_, s, _, p) ->
	fold_asig_paths f s *> f `Structure p
and fold_adec_paths f = function
    | Adec_include (_, s) -> fold_asig_paths f s
    | Adec_open (_, p) -> f `Signature p
    | Adec_in (_, _, s) -> fold_asig_paths f s
    | Adec_sig (_, _, s) -> Option.fold (fold_asig_paths f) s
    | Adec_types bindings -> List.fold (fold_atypbind_paths f) bindings
    | Adec_val (_, _, u) -> fold_atyp_paths (f `Type) u

let rec fold_amod_paths f = function
    | Amod_ref p -> f `Structure p
    | Amod_defs (_, defs) -> List.fold (fold_adef_paths f) defs
    | Amod_apply (_, m0, m1) -> fold_amod_paths f m0 *> fold_amod_paths f m1
    | Amod_lambda (_, _, s, m) -> fold_asig_paths f s *> fold_amod_paths f m
    | Amod_coercion (_, m, s) -> fold_amod_paths f m *> fold_asig_paths f s
and fold_adef_paths f = function
    | Adef_include (_, m) -> fold_amod_paths f m
    | Adef_open (_, p) -> f `Structure p
    | Adef_in (_, _, m) -> fold_amod_paths f m
    | Adef_sig (_, _, s) -> fold_asig_paths f s
    | Adef_types bindings -> List.fold (fold_atypbind_paths f) bindings
    | Adef_val (_, _, x) -> fold_aval_paths (f `Value) x
    | Adef_vals bindings ->
	List.fold (fun (_, _, x) -> fold_aval_paths (f `Value) x) bindings
