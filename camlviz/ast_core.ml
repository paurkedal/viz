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

open Ast_types
open Cst_types
open Leaf_types
open Leaf_core
open FfPervasives
open Unicode

let avar_idr (Avar (_, idr)) = idr
let avar_name (Avar (_, Idr s)) = s
let avar_loc (Avar (loc, _)) = loc

let ascii_encode s =
    let buf = UString.Buf.create 16 in
    let s' =
	if String.length s >= 2 then
	    match String.sub s 0 2 with
	    | "0'" -> "op0_" ^ (String.after 2 s)
	    | "1'" -> "op1_" ^ (String.after 2 s)
	    | "2'" -> "op2_" ^ (String.after 2 s)
	    | _ -> s
	else s in
    UString.iter
	begin fun ch ->
	    if UChar.is_ocaml_idrcnt ch then UString.Buf.add_char buf ch else
	    let s = Printf.sprintf "U%04x" (UChar.uint_code ch) in
	    UString.Buf.add_string buf (UString.of_utf8 s)
	end
	(UString.of_utf8 s');
    UString.to_utf8 (UString.Buf.contents buf)

let str_to_lid s = String.uncapitalize (ascii_encode s)
let str_to_uid s =
    assert (String.length s > 0);
    if String.get s 0 = '_' then "Z_" ^ s else
    String.capitalize (ascii_encode s)
let idr_to_lid (Idr s) = str_to_lid s
let idr_to_uid (Idr s) = str_to_uid s
let avar_to_lid (Avar (_, idr)) = idr_to_lid idr
let avar_to_uid (Avar (_, idr)) = idr_to_uid idr

let fresh_avar_at ?(prefix = "_x") loc =
    let chno = Location.Bound.charno (Location.lbound loc) in
    Avar (loc, Idr (prefix ^ (string_of_int chno)))

let apath_loc (Apath (loc, _)) = loc

let atyp_loc = function
    | Atyp_ref p -> apath_loc p
    | Atyp_uvar v -> avar_loc v
    | Atyp_A (loc, _, _) -> loc
    | Atyp_E (loc, _, _) -> loc
    | Atyp_apply (loc, _, _) -> loc
    | Atyp_arrow (loc, _, _) -> loc

let rec aval_loc = function
    | Aval_literal (loc, _) -> loc
    | Aval_ref p -> apath_loc p
    | Aval_apply (loc, _, _) -> loc
    | Aval_array (loc, _) -> loc
    | Aval_at (loc, _) -> loc
    | Aval_let (loc, _, _, _) -> loc
    | Aval_letrec (loc, _, _) -> loc
    | Aval_if (loc, _, _, _) -> loc
    | Aval_back loc -> loc
    | Aval_match (loc, _, _) -> loc
    | Aval_seq (loc, _, _, _) -> loc
    | Aval_raise (loc, _) -> loc

let rec apat_loc = function
    | Apat_literal (loc, _) -> loc
    | Apat_ref p -> apath_loc p
    | Apat_uvar x -> avar_loc x
    | Apat_apply (loc, _, _) -> loc
    | Apat_as (loc, _, _) -> loc
    | Apat_intype (loc, _, _) -> loc

let rec asig_loc = function
    | Asig_ref p -> apath_loc p
    | Asig_decs (loc, bs) -> loc
    | Asig_product (loc, _, _, _) -> loc
    | Asig_suspension (loc, _) -> loc
    | Asig_with_type (loc, _, _, _) -> loc
    | Asig_with_struct (loc, _, _, _) -> loc
and adec_loc = function
    | Adec_include (loc, _) -> loc
    | Adec_open (loc, _) -> loc
    | Adec_use (loc, _) -> loc
    | Adec_in (loc, _, _) -> loc
    | Adec_sig (loc, _, _) -> loc
    | Adec_types bs ->
	let (lloc, _, _, _) = List.hd bs in
	let (uloc, _, _, _) = List.last bs in
	Location.span [lloc; uloc]
    | Adec_injx (loc, _, _) -> loc
    | Adec_val (loc, _, _) -> loc
    | Adec_cabi_val (loc, _, _, _, _) -> loc

and amod_loc = function
    | Amod_ref p -> apath_loc p
    | Amod_defs (loc, _) -> loc
    | Amod_apply (loc, _, _) -> loc
    | Amod_lambda (loc, _, _, _) -> loc
    | Amod_suspend (loc, _) -> loc
    | Amod_generate (loc, _) -> loc
    | Amod_coercion (loc, _, _) -> loc
and adef_loc = function
    | Adef_include (loc, _) -> loc
    | Adef_open (loc, _) -> loc
    | Adef_use (loc, _) -> loc
    | Adef_in (loc, _, _) -> loc
    | Adef_sig (loc, _, _) -> loc
    | Adef_types bs ->
	let (lloc, _, _, _) = List.hd bs in
	let (uloc, _, _, _) = List.last bs in
	Location.span [lloc; uloc]
    | Adef_injx (loc, _, _) -> loc
    | Adef_let (loc, _, _) -> loc
    | Adef_letrec bs ->
	let (lloc, _, _, _) = List.hd bs in
	let (uloc, _, _, _) = List.last bs in
	Location.span [lloc; uloc]
    | Adef_cabi_val (loc, _, _, _, _) -> loc
    | Adef_cabi_open (loc, _) -> loc

let apath_to_idr (Apath (loc, p)) =
    assert (Modpath.is_atom p); Modpath.last_e p
let apath_eq_idr idr (Apath (loc, p)) =
    Modpath.is_atom p && Modpath.last_e p = idr
let apath_eq_string s (Apath (loc, p)) =
    Modpath.is_atom p && Modpath.last_e p = Idr s
let apath_of_avar (Avar (loc, v)) = Apath (loc, Modpath.atom v)

let apat_uvar_any loc = Apat_uvar (Avar (loc, Idr "_"))
let apat_uvar_of_idr loc idr = Apat_uvar (Avar (loc, idr))
let aval_ref_of_idr loc idr = Aval_ref (Apath (loc, Modpath.atom idr))

let aval_string loc s =
    Aval_literal (loc, Lit_string (UString.of_utf8 s))

let aval_apply1i loc idr x =
    Aval_apply (loc, Aval_ref (Apath (loc, Modpath.atom idr)), x)
let aval_apply2i loc idr x y =
    Aval_apply (loc, aval_apply1i loc idr x, y)

let aval_internal_error loc msg =
    aval_apply2i loc (Idr "__failure")
		 (aval_string loc (Location.to_string loc))
		 (aval_string loc msg)

let aval_map_subaval f = function
    | Aval_literal _ | Aval_ref _ | Aval_back _ as x -> x
    | Aval_apply (loc, x, y) -> Aval_apply (loc, f x, f y)
    | Aval_array (loc, xs) -> Aval_array (loc, List.map f xs)
    | Aval_at (loc, cases) ->
	let g (p, xopt, y) = (p, Option.map f xopt, f y) in
	Aval_at (loc, List.map g cases)
    | Aval_match (loc, x, cases) ->
	let g (p, xopt, y) = (p, Option.map f xopt, f y) in
	Aval_match (loc, f x, List.map g cases)
    | Aval_let (loc, p, x, y) -> Aval_let (loc, p, f x, f y)
    | Aval_letrec (loc, cases, x) ->
	let g (loc, v, topt, x) = (loc, v, topt, f x) in
	Aval_letrec (loc, List.map g cases, f x)
    | Aval_if (loc, x, y, z) -> Aval_if (loc, f x, f y, f z)
    | Aval_seq (loc, op, x, y) -> Aval_seq (loc, op, f x, Option.map f y)
    | Aval_raise (loc, x) -> Aval_raise (loc, f x)
let adef_map_subamod f = function
    | Adef_include (loc, m) -> Adef_include (loc, f m)
    | Adef_in (loc, v, m) -> Adef_in (loc, v, f m)
    | Adef_open _ | Adef_use _ | Adef_sig _
    | Adef_types _ | Adef_injx _ | Adef_let _ | Adef_letrec _
    | Adef_cabi_open _ | Adef_cabi_val _
	as def -> def
let amod_map_subamod f = function
    | Amod_ref _ as m -> m
    | Amod_defs (loc, defs) ->
	Amod_defs (loc, List.map (adef_map_subamod f) defs)
    | Amod_apply (loc, m0, m1) -> Amod_apply (loc, f m0, f m1)
    | Amod_lambda (loc, v, s, m) -> Amod_lambda (loc, v, s, f m)
    | Amod_suspend (loc, m) -> Amod_suspend (loc, f m)
    | Amod_generate (loc, m) -> Amod_generate (loc, f m)
    | Amod_coercion (loc, m, s) -> Amod_coercion (loc, f m, s)
let adef_map_subaval f = function
    | Adef_include _ | Adef_open _ | Adef_in _ | Adef_sig _
    | Adef_types _ | Adef_injx _ | Adef_cabi_open _ | Adef_cabi_val _
	as def -> def
    | Adef_use (loc, x) -> Adef_use (loc, f x)
    | Adef_let (loc, p, x) -> Adef_let (loc, p, f x)
    | Adef_letrec cases ->
	let g (loc, v, topt, x) = (loc, v, topt, f x) in
	Adef_letrec (List.map g cases)
let amod_map_subaval f = function
    | Amod_defs (loc, defs) ->
	Amod_defs (loc, List.map (adef_map_subaval f) defs)
    | Amod_ref _ | Amod_apply _ | Amod_lambda _
    | Amod_suspend _ | Amod_generate _ | Amod_coercion _ as m -> m
