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

open Leaf_types
open Ast_core
open Ast_types
open Diag
open Sexplib
open FfPervasives
open Unicode

let sexp_to_string sx =
    let buf = Buffer.create 64 in
    Sexp.to_buffer_hum buf sx;
    Buffer.contents buf
let atyp_to_string x = sexp_to_string (sexp_of_atyp x)
let apat_to_string x = sexp_to_string (sexp_of_apat x)
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

let arity t = fold_arg_types (fun _ accu -> accu + 1) t 0

type pocket =
    | No_pocket
    | Local_pocket of avar
    | World_pocket

let atyp_action_pocket = function
    | Atyp_ref (Apath ([], Avar (_, Idr io))) when io = "io" ->
	World_pocket
    | Atyp_apply (_, Atyp_ref (Apath ([], Avar (_, Idr action))), pocket) ->
	begin match pocket with
	| Atyp_ref (Apath ([], Avar (_, Idr phi))) when phi = "world_pocket" ->
	    World_pocket
	| Atyp_uvar v ->
	    Local_pocket v
	| _ ->
	    errf_at (atyp_loc pocket) "Invalid pocket type tag."
	end
    | _ -> No_pocket
let unwrap_atyp_action = function
    | Atyp_apply (_, u, v) as t ->
	begin match atyp_action_pocket u with
	| No_pocket -> (No_pocket, t)
	| p -> (p, v)
	end
    | t -> (No_pocket, t)

let atyp_is_const = function
    | Atyp_arrow _ -> false
    | Atyp_apply (_, u, v) ->
	begin match atyp_action_pocket u with
	| No_pocket -> true
	| _ -> false
	end
    | _ -> true

let flatten_arrows =
    let rec loop ats = function
	| Atyp_arrow (_, at, rt) -> loop (at :: ats) rt
	| rt -> (rt, List.rev ats) in
    loop []

let flatten_arrows_for_c t =
    let rt, ats = flatten_arrows t in
    let pocket, rt = unwrap_atyp_action rt in
    if pocket <> No_pocket && ats = [] then
	let loc = atyp_loc t in
	(true, rt, [Atyp_ref (Apath ([], Avar (loc, Idr "unit")))])
    else
	(false, rt, ats)

let flatten_application =
    let rec loop args = function
	| Atyp_apply (_, at, arg) -> loop (arg :: args) at
	| Atyp_ref p -> (p, List.rev args)
	| at -> errf_at (atyp_loc at) "Expecting a type constructor." in
    loop []


(* Folds over Variable Subterms *)

let rec fold_apat_vars f = function
    | Apat_literal _ | Apat_ref _ -> ident
    | Apat_uvar v -> f v
    | Apat_apply (_, p, q) -> fold_apat_vars f p *> fold_apat_vars f q
    | Apat_intype (_, t, p) -> fold_apat_vars f p

let rec fold_apat_typed_vars f = function
    | Apat_literal _ | Apat_ref _ -> ident
    | Apat_uvar v -> ident
    | Apat_apply (_, p, q) ->
	fold_apat_typed_vars f p *> fold_apat_typed_vars f q
    | Apat_intype (_, t, Apat_uvar v) -> f (t, v)
    | Apat_intype (_, _, p) -> fold_apat_typed_vars f p


(* Folds over Path Subterms *)

let rec fold_atyp_paths f = function
    | Atyp_ref p -> f p
    | Atyp_uvar _ -> ident
    | Atyp_apply (_, u, v) | Atyp_arrow (_, u, v) ->
	fold_atyp_paths f u *> fold_atyp_paths f v

let rec fold_apat_paths f = function
    | Apat_literal _ -> ident
    | Apat_ref p -> f `Value p
    | Apat_uvar _ -> ident
    | Apat_apply (_, p, q) ->
	fold_apat_paths f p *> fold_apat_paths f q
    | Apat_intype (_, t, p) ->
	fold_atyp_paths (f `Type) t *< fold_apat_paths f p

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
    | Aval_ref p -> f `Value p
    | Aval_apply (_, x, y) -> fold_aval_paths f x *> fold_aval_paths f y
    | Aval_array (_, xs) -> List.fold (fold_aval_paths f) xs
    | Aval_at (_, cases) -> fold_cases cases
    | Aval_match (_, x, cases) -> fold_aval_paths f x *> fold_cases cases
    | Aval_let (_, p, rhs, body) ->
	fold_apat_paths f p *> fold_aval_paths f rhs *> fold_aval_paths f body
    | Aval_letrec (_, bindings, body) ->
	List.fold
	    (fun (_, v, topt, x) ->
		Option.fold (fold_atyp_paths (f `Type)) topt *>
		fold_aval_paths f x)
	    bindings *>
	fold_aval_paths f body
    | Aval_if (_, c, cq, ccq) ->
	fold_aval_paths f c *> fold_aval_paths f cq *> fold_aval_paths f ccq
    | Aval_assert (_, x, y) -> fold_aval_paths f x *> fold_aval_paths f y
    | Aval_raise (_, x) -> fold_aval_paths f x

let fold_atypinfo_paths f = function
    | Atypinfo_abstract | Atypinfo_cabi _ -> ident
    | Atypinfo_alias u -> fold_atyp_paths f u
    | Atypinfo_injs injs ->
	List.fold (fun (_, _, u, _) -> fold_atyp_paths f u) injs

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
    | Adec_use _ -> ident
    | Adec_in (_, _, s) -> fold_asig_paths f s
    | Adec_sig (_, _, s) -> Option.fold (fold_asig_paths f) s
    | Adec_types bindings -> List.fold (fold_atypbind_paths f) bindings
    | Adec_val (_, _, t) -> fold_atyp_paths (f `Type) t
    | Adec_cabi_val (_, _, t, _, _) -> fold_atyp_paths (f `Type) t

let rec fold_amod_paths ?module_name f = function
    | Amod_ref p -> f `Structure p
    | Amod_defs (_, defs) -> List.fold (fold_adef_paths ?module_name f) defs
    | Amod_apply (_, m0, m1) ->
	fold_amod_paths ?module_name f m0 *> fold_amod_paths ?module_name f m1
    | Amod_lambda (_, _, s, m) ->
	fold_asig_paths f s *> fold_amod_paths ?module_name f m
    | Amod_coercion (_, m, s) ->
	fold_amod_paths ?module_name f m *> fold_asig_paths f s
and fold_adef_paths ?module_name f = function
    | Adef_include (_, m) -> fold_amod_paths ?module_name f m
    | Adef_open (_, p) -> f `Structure p
    | Adef_use _ -> ident
    | Adef_in (_, _, m) -> fold_amod_paths ?module_name f m
    | Adef_sig (_, _, s) -> fold_asig_paths f s
    | Adef_types bindings -> List.fold (fold_atypbind_paths f) bindings
    | Adef_let (_, p, x) -> fold_apat_paths f p *> fold_aval_paths f x
    | Adef_letrec bindings ->
	List.fold
	    (fun (_, _, t, x) ->
		Option.fold (fold_atyp_paths (f `Type)) t *>
		fold_aval_paths f x)
	    bindings
    | Adef_cabi_val (_, v, t, _, _) ->
	(if atyp_is_const t then
	    match module_name with
	    | Some mname ->
		f `Value
		  (Apath ([Avar (Location.dummy, Idr (mname ^ "_FFIC"))], v))
	    | None -> ident else
	ident) *> fold_atyp_paths (f `Type) t
    | Adef_cabi_open _ -> ident

let rec fold_amod_cabi_open f = function
    | Amod_ref _ -> ident
    | Amod_defs (_, defs) -> List.fold (fold_adef_cabi_open f) defs
    | Amod_apply (_, mf, ma) ->
	fold_amod_cabi_open f mf *> fold_amod_cabi_open f ma
    | Amod_lambda (_, _, _, m) | Amod_coercion (_, m, _) ->
	fold_amod_cabi_open f m
and fold_adef_cabi_open f = function
    | Adef_include (_, m) | Adef_in (_, _, m) ->
	fold_amod_cabi_open f m
    | Adef_open _ | Adef_use _ | Adef_sig _ | Adef_types _
    | Adef_let _ | Adef_letrec _ ->
	ident
    | Adef_cabi_val _ -> ident
    | Adef_cabi_open (_, inc) -> f inc

let interpret_use use =
    let rec flatten params = function
	| Aval_apply (_, inner, param) -> flatten (param :: params) inner
	| directive -> directive, params in
    let directive, params = flatten [] use in
    match directive with
    | Aval_ref (Apath ([Avar (_, Idr "cabi")], subdirective)) ->
	begin match subdirective with
	| Avar (_, Idr "stub_prefix") ->
	    begin match params with
	    | [Aval_literal (_, Lit_string pfx)] ->
		`Stub_prefix (UString.to_utf8 pfx)
	    | _ -> errf_at (aval_loc use) "Invalid stub prefix."
	    end
	| _ -> errf_at (avar_loc subdirective) "Invalid cabi use-directive."
	end
    | _ ->
	errf_at (aval_loc directive) "Invalid use-directive."
