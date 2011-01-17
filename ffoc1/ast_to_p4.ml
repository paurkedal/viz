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

module Ast = Camlp4.PreCast.Ast
module Loc = Camlp4.PreCast.Loc
open Cst_types
open Ast_types
open Ast_core
open Diag
open FfPervasives
open Unicode

let p4loc loc =
    let lb = Location.lbound loc in
    let ub = Location.ubound loc in
    Loc.of_tuple
        (Location.path loc,
         Location.Bound.lineno lb,
         Location.Bound.bol_charno lb,
         Location.Bound.charno lb,
         Location.Bound.lineno ub,
         Location.Bound.bol_charno ub,
         Location.Bound.charno ub,
         true)

let ascii_encode s =
    let buf = UString.Buf.create 16 in
    let s' =
	if String.length s >= 2 then
	    match String.sub s 0 2 with
	    | "1'" -> "_1o_" ^ (String.after 2 s)
	    | "2'" -> "_2o_" ^ (String.after 2 s)
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
let str_to_uid s = String.capitalize (ascii_encode s)
let idr_to_lid (Idr s) = str_to_lid s
let idr_to_uid (Idr s) = str_to_uid s
let avar_to_lid (Avar (_, idr)) = idr_to_lid idr
let avar_to_uid (Avar (_, idr)) = idr_to_uid idr

let rec emit_apath_helper inner_loc final = function
    | [] -> final
    | av :: avs ->
	let loc = Location.span [avar_loc av; inner_loc] in
	let _loc = p4loc loc in
	let ov = <:ident< $uid: avar_to_uid av$ >> in
	<:ident< $emit_apath_helper inner_loc ov avs$ . $final$ >>

let emit_apath_lid (Apath (vs, v)) =
    let _loc = p4loc (avar_loc v) in
    emit_apath_helper (avar_loc (List.last (v :: vs)))
		      <:ident< $lid: avar_to_lid v$ >> vs
let emit_apath_uid (Apath (vs, v)) =
    let _loc = p4loc (avar_loc v) in
    emit_apath_helper (avar_loc (List.last (v :: vs)))
		      <:ident< $uid: avar_to_uid v$ >> vs

let rec emit_atyp = function
    | Atyp_uvar v ->
	let _loc = p4loc (avar_loc v) in
	<:ctyp< '$lid: avar_to_lid v$ >>
    | Atyp_ref p ->
	let _loc = p4loc (apath_loc p) in
	<:ctyp< $id: emit_apath_lid p$ >>
    | Atyp_apply (loc, at, au) ->
	let _loc = p4loc loc in
	<:ctyp< $emit_atyp at$ $emit_atyp au$ >>
    | Atyp_arrow (loc, at, au) ->
	let _loc = p4loc loc in
	<:ctyp< $emit_atyp at$ -> $emit_atyp au$ >>

let emit_apat_literal loc lit =
    let _loc = p4loc loc in
    match lit with
    | Lit_unit     -> <:patt< () >>
    | Lit_bool x   -> if x then <:patt< True >> else <:patt< False >>
    | Lit_int x    -> let s = string_of_int x in <:patt< $int:s$ >>
    | Lit_float x  -> let s = string_of_float x in <:patt< $flo:s$ >>
    | Lit_string x -> let s = UString.to_utf8 x in <:patt< $str:s$ >>

let emit_aval_literal loc lit =
    let _loc = p4loc loc in
    match lit with
    | Lit_unit     -> <:expr< () >>
    | Lit_bool x   -> if x then <:expr< True >> else <:expr< False >>
    | Lit_int x    -> let s = string_of_int x in <:expr< $int:s$ >>
    | Lit_float x  -> let s = string_of_float x in <:expr< $flo:s$ >>
    | Lit_string x -> let s = UString.to_utf8 x in <:expr< $str:s$ >>

let rec emit_apat = function
    | Apat_ref p ->
	let _loc = p4loc (apath_loc p) in
	<:patt< $id: emit_apath_lid p$ >>
    | Apat_uvar v ->
	let _loc = p4loc (avar_loc v) in
	<:patt< $lid: avar_to_lid v$ >>
    | Apat_apply (loc, p0, p1) ->
	let _loc = p4loc loc in
	<:patt< $emit_apat p0$ $emit_apat p1$ >>

let rec emit_aval = function
    | Aval_literal (loc, lit) -> emit_aval_literal loc lit
    | Aval_ref p ->
	let _loc = p4loc (apath_loc p) in
	<:expr< $id: emit_apath_lid p$ >>
    | Aval_apply (loc, x, y) ->
	let _loc = p4loc loc in
	<:expr< $emit_aval x$ $emit_aval y$ >>
    | Aval_at (loc, cases) ->
	let _loc = p4loc loc in
	<:expr< fun [ $list: List.map emit_match_case cases$ ] >>
    | Aval_match (loc, x, cases) ->
	let _loc = p4loc loc in
	<:expr< match $emit_aval x$
		with [ $list: List.map emit_match_case cases$ ] >>
    | Aval_let (loc, bindings, body) ->
	let _loc = p4loc loc in
	let emit_binding (loc, x, body) =
	    let _loc = p4loc loc in
	    <:binding< $pat: emit_apat x$ = $emit_aval body$ >> in
	<:expr< let rec $list: List.map emit_binding bindings$
		in $emit_aval body$ >>
    | Aval_if (loc, cond, cq, ccq) ->
	let _loc = p4loc loc in
	<:expr< if $emit_aval cond$ then $emit_aval cq$ else $emit_aval ccq$ >>
    | Aval_raise (loc, x) ->
	let _loc = p4loc loc in
	<:expr< raise $emit_aval x$ >>
and emit_match_case (pat, cond_opt, body) =
    let opat = emit_apat pat in
    let obody = emit_aval body in
    let _loc = p4loc (Location.span [apat_loc pat; aval_loc body]) in
    match cond_opt with
    | Some cond ->
	let ocond = emit_aval cond in
	<:match_case< $pat: opat$ when $ocond$ -> $obody$ >>
    | None ->
	<:match_case< $pat: opat$ -> $obody$ >>

let emit_type_binding (loc, v, params, ti) =
    let _loc = p4loc loc in
    let rhs =
	match ti with
	| Atypinfo_alias typ -> emit_atyp typ
	| Atypinfo_injs injs ->
	    let emit_inj (loc, v, at) =
		let _loc = p4loc loc in
		let _, ats = Ast_utils.flatten_application at in
		let ots = List.map emit_atyp ats in
		<:ctyp< $uid: avar_to_uid v$ of $list: ots$ >> in
	    <:ctyp< [ $list: List.map emit_inj injs$ ] >> in
    Ast.TyDcl (_loc, avar_to_lid v, List.map emit_atyp params, rhs, [])

let rec emit_asig = function
    | Asig_ref p ->
	let _loc = p4loc (apath_loc p) in
	<:module_type< $id: emit_apath_uid p$ >>
    | Asig_decs (loc, decs) ->
	let _loc = p4loc loc in
	<:module_type< sig $list: List.map emit_adec decs$ end >>
    | Asig_product (loc, xv, xsig, ysig) ->
	let _loc = p4loc loc in
	<:module_type<
	    functor ( $uid: avar_to_uid xv$ : $emit_asig xsig$ ) ->
		$emit_asig ysig$ >>
    | Asig_with_type (loc, _, _) | Asig_with_sig (loc, _, _) ->
	errf_at loc "UNIMPLEMENTED"
and emit_adec = function
    | Adec_include (loc, s) ->
	let _loc = p4loc loc in
	<:sig_item< include $emit_asig s$ >>
    | Adec_open (loc, p) ->
	let _loc = p4loc loc in
	<:sig_item< open $id: emit_apath_uid p$ >>
    | Adec_in (loc, v, s) ->
	let _loc = p4loc loc in
	<:sig_item< module $uid: avar_to_uid v$ : $emit_asig s$ >>
    | Adec_sig (loc, v, None) ->
	let _loc = p4loc loc in
	<:sig_item< module type $uid: avar_to_uid v$ >>
    | Adec_sig (loc, v, Some s) ->
	let _loc = p4loc loc in
	<:sig_item< module type $uid: avar_to_uid v$ = $emit_asig s$ >>
    | Adec_types bindings ->
	let (lloc, _, _, _) = List.hd bindings in
	let (uloc, _, _, _) = List.last bindings in
	let _loc = p4loc (Location.span [lloc; uloc]) in
	<:sig_item< type $list: List.map emit_type_binding bindings$ >>
    | Adec_val (loc, xv, xt) ->
	let _loc = p4loc loc in
	<:sig_item< value $lid: avar_to_lid xv$ : $emit_atyp xt$ >>

let rec emit_amod = function
    | Amod_ref p ->
	let _loc = p4loc (apath_loc p) in
	<:module_expr< $id: emit_apath_uid p$ >>
    | Amod_defs (loc, defs) ->
	let _loc = p4loc loc in
	<:module_expr< struct $list: List.map emit_adef defs$ end >>
    | Amod_apply (loc, x, y) ->
	let _loc = p4loc loc in
	<:module_expr< $emit_amod x$ $emit_amod y$ >>
    | Amod_lambda (loc, xv, xsig, ymod) ->
	let _loc = p4loc loc in
	<:module_expr<
	    functor ($uid: avar_to_uid xv$ : $emit_asig xsig$) ->
		$emit_amod ymod$ >>
    | Amod_coercion (loc, m, s) ->
	let _loc = p4loc loc in
	<:module_expr< ($emit_amod m$ : $emit_asig s$) >>
and emit_adef = function
    | Adef_include (loc, m) ->
	let _loc = p4loc loc in
	<:str_item< include $emit_amod m$ >>
    | Adef_open (loc, p) ->
	let _loc = p4loc loc in
	<:str_item< open $id: emit_apath_uid p$ >>
    | Adef_in (loc, v, m) ->
	let _loc = p4loc loc in
	<:str_item< module $uid: avar_to_uid v$ = $emit_amod m$ >>
    | Adef_sig (loc, v, s) ->
	let _loc = p4loc loc in
	<:str_item< module type $uid: avar_to_uid v$ = $emit_asig s$ >>
    | Adef_types bindings ->
	let (lloc, _, _, _) = List.hd bindings in
	let (uloc, _, _, _) = List.last bindings in
	let _loc = p4loc (Location.span [lloc; uloc]) in
	<:str_item< type $list: List.map emit_type_binding bindings$ >>
    | Adef_vals bindings ->
	let (lloc, _, _) = List.hd bindings in
	let (uloc, _, _) = List.last bindings in
	let _loc = p4loc (Location.span [lloc; uloc]) in
	let emit_value_binding (loc, v, x) =
	    let _loc = p4loc loc in
	    <:binding< $lid: avar_to_lid v$ = $emit_aval x$ >> in
	<:str_item< value rec $list: List.map emit_value_binding bindings$ >>

let emit_toplevel = function
    | Amod_defs (loc, defs) ->
	<:str_item< $list: List.map emit_adef defs$ >>
    | amod ->
	errf_at (amod_loc amod) "Module expression not allowed at top-level."
