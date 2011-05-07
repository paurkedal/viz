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

module Ast = Camlp4.PreCast.Ast
module Loc = Camlp4.PreCast.Loc
open Leaf_types
open Leaf_core
open Cst_types
open Cst_core
open Ast_types
open Ast_core
open Diag
open FfPervasives
open Unicode

let apath_is idr = function
    | Apath ([], Avar (_, idr')) -> idr = idr'
    | _ -> false

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

type typefor = Typefor_viz | Typefor_cabi | Typefor_cabi_io

let rec emit_atyp ?(typefor = Typefor_viz) = function
    | Atyp_uvar v ->
	let _loc = p4loc (avar_loc v) in
	<:ctyp< '$lid: avar_to_lid v$ >>
    | Atyp_ref p ->
	let _loc = p4loc (apath_loc p) in
	<:ctyp< $id: emit_apath_lid p$ >>
    | Atyp_A (loc, v, t) | Atyp_E (loc, v, t) ->
	warnf_at loc "Ignoring quantification of %s." (avar_name v);
	emit_atyp ~typefor t
    | Atyp_apply (loc, Atyp_apply (_, Atyp_ref (Apath ([], op)), t), u)
	    when avar_idr op = Cst_core.idr_2o_times ->
	let _loc = p4loc loc in
	<:ctyp< ($emit_atyp t$ * $emit_atyp u$) >>
    | Atyp_apply (loc, Atyp_apply (_, Atyp_ref (Apath ([], op)), t), u)
	    when (typefor = Typefor_cabi || typefor = Typefor_cabi_io)
	      && avar_idr op = Cst_core.idr_2o_index ->
	emit_atyp ~typefor t
    | Atyp_apply (loc, at, au) ->
	if typefor = Typefor_cabi_io
		&& Ast_utils.atyp_action_pocket at <> Ast_utils.No_pocket then
	    emit_atyp ~typefor au
	else
	    let _loc = p4loc loc in
	    <:ctyp< $emit_atyp ~typefor at$ $emit_atyp ~typefor au$ >>
    | Atyp_arrow (loc, at, au) ->
	let _loc = p4loc loc in
	<:ctyp< $emit_atyp ~typefor at$ -> $emit_atyp ~typefor au$ >>

let emit_atyp_cabi_io = function
    | Atyp_arrow _ as t ->
	emit_atyp ~typefor: Typefor_cabi_io t
    | t ->
	let _loc = p4loc (atyp_loc t) in
	<:ctyp< unit -> $emit_atyp ~typefor: Typefor_cabi_io t$ >>

let emit_apat_literal loc lit ocond_opt =
    let _loc = p4loc loc in
    match lit with
    | Lit_unit     -> <:patt< () >>, ocond_opt
    | Lit_bool x   -> (if x then <:patt<True>> else <:patt<False>>), ocond_opt
    | Lit_int x    -> let s = string_of_int x in <:patt< $int:s$ >>, ocond_opt
    | Lit_float x  -> let s = string_of_float x in <:patt<$flo:s$>>, ocond_opt
    | Lit_char x   ->
	let v = fresh_avar_at loc in
	let cond = <:expr< __generic_eq $lid: avar_to_lid v$
			    (__char_of_utf8 $str: UChar.to_utf8 x$) >> in
	let cond =
	    match ocond_opt with
	    | None -> cond
	    | Some cond' -> <:expr< $cond'$ && $cond$ >> in
	<:patt< $lid: avar_to_lid v$ >>, Some cond
    | Lit_string x ->
	let v = fresh_avar_at loc in
	let cond = <:expr< __generic_eq $lid: avar_to_lid v$
			    (__string_of_utf8 $str: UString.to_utf8 x$) >> in
	let cond =
	    match ocond_opt with
	    | None -> cond
	    | Some cond' -> <:expr< $cond'$ && $cond$ >> in
	<:patt< $lid: avar_to_lid v$ >>, Some cond

let emit_aval_literal loc lit =
    let _loc = p4loc loc in
    match lit with
    | Lit_unit     -> <:expr< () >>
    | Lit_bool x   -> if x then <:expr< True >> else <:expr< False >>
    | Lit_int x    -> let s = string_of_int x in <:expr< $int:s$ >>
    | Lit_float x  -> let s = string_of_float x in <:expr< $flo:s$ >>
    | Lit_char x   -> <:expr< __char_of_utf8 $str: UChar.to_utf8 x$ >>
    | Lit_string x -> <:expr< __string_of_utf8 $str: UString.to_utf8 x$ >>

let emit_apat_fixed _loc default = function
    | "()" | "0'(')" -> <:patt< () >>
    | "[]" | "0'[']" -> <:patt< [] >>
    | "#[]" | "0'#[']" -> <:patt< [| |] >>
    | _ -> default ()

let rec emit_apat = function
    | Apat_literal (loc, lit) -> fun ocond_opt ->
	emit_apat_literal loc lit ocond_opt
    | Apat_ref p -> fun ocond_opt ->
	let _loc = p4loc (apath_loc p) in
	let default () = <:patt< $id: emit_apath_uid p$ >> in
	begin match p with
	| Apath ([], Avar (_, Idr s)) -> emit_apat_fixed _loc default s
	| _ -> default ()
	end, ocond_opt
    | Apat_uvar v ->
	let _loc = p4loc (avar_loc v) in
	fun ocond_opt -> <:patt< $lid: avar_to_lid v$ >>, ocond_opt
    | Apat_apply (loc, Apat_apply (_, Apat_ref op, x), y)
	    when apath_is Cst_core.idr_list_push op -> fun ocond_opt ->
	let _loc = p4loc loc in
	let ox, ocond_opt = emit_apat x ocond_opt in
	let oy, ocond_opt = emit_apat y ocond_opt in
	<:patt< [$ox$ :: $oy$] >>, ocond_opt
    | Apat_apply (loc, Apat_apply (_, Apat_ref op, x), y)
	    when apath_is Cst_core.idr_2o_comma op -> fun ocond_opt ->
	let _loc = p4loc loc in
	let ox, ocond_opt = emit_apat x ocond_opt in
	let oy, ocond_opt = emit_apat y ocond_opt in
	<:patt< ($ox$, $oy$) >>, ocond_opt
    | Apat_apply (loc, x, y) -> fun ocond_opt ->
	let _loc = p4loc loc in
	let ox, ocond_opt = emit_apat x ocond_opt in
	let oy, ocond_opt = emit_apat y ocond_opt in
	<:patt< $ox$ $oy$ >>, ocond_opt
    | Apat_as (loc, v, x) -> fun ocond_opt ->
	let _loc = p4loc loc in
	let ox, ocond_opt = emit_apat x ocond_opt in
	<:patt< ($ox$ as $lid: avar_to_lid v$) >>, ocond_opt
    | Apat_intype (loc, t, x) -> fun ocond_opt ->
	let _loc = p4loc loc in
	let ox, ocond_opt = emit_apat x ocond_opt in
	<:patt< ($ox$ : $emit_atyp t$) >>, ocond_opt

let emit_aval_fixed _loc default = function
    | "()" | "0'(')" -> <:expr< () >>
    | "[]" | "0'[']" -> <:expr< [] >>
    | "#[]" | "0'#[']" -> <:expr< [| |] >>
    | "[;]" -> <:expr< Data.List.push >>
    | _ -> default ()

let rec emit_aval = function
    | Aval_literal (loc, lit) -> emit_aval_literal loc lit
    | Aval_ref p ->
	let _loc = p4loc (apath_loc p) in
	let default () = <:expr< $id: emit_apath_lid p$ >> in
	begin match p with
	| Apath ([], Avar (_, Idr s)) -> emit_aval_fixed _loc default s
	| _ -> default ()
	end
    | Aval_apply (loc, Aval_apply (_, Aval_ref op, x), y)
	    when apath_is Cst_core.idr_list_push op ->
	let _loc = p4loc loc in
	<:expr< [$emit_aval x$ :: $emit_aval y$] >>
    | Aval_apply (loc, Aval_apply (_, Aval_ref op, x), y)
	    when apath_is Cst_core.idr_2o_comma op ->
	let _loc = p4loc loc in
	<:expr< ($emit_aval x$, $emit_aval y$) >>
    | Aval_apply (loc, x, y) ->
	let _loc = p4loc loc in
	<:expr< $emit_aval x$ $emit_aval y$ >>
    | Aval_array (loc, xs) ->
	let _loc = p4loc loc in
	<:expr< [| $list: List.map emit_aval xs$ |] >>
    | Aval_at (loc, cases) ->
	let _loc = p4loc loc in
	<:expr< fun [ $list: List.map emit_match_case cases$ ] >>
    | Aval_match (loc, x, cases) ->
	let _loc = p4loc loc in
	<:expr< match $emit_aval x$
		with [ $list: List.map emit_match_case cases$ ] >>
    | Aval_let (loc, p, x, body) ->
	let _loc = p4loc loc in
	let op, ocond_opt = emit_apat p None in
	if ocond_opt <> None then
	    errf_at loc "Cannot match string literals in let-binding.";
	<:expr< let $pat: op$ = $emit_aval x$ in $emit_aval body$ >>
    | Aval_letrec (loc, bindings, body) ->
	let _loc = p4loc loc in
	let emit_binding (loc, v, topt, body) =
	    let _loc = p4loc loc in
	    match topt with
	    | None ->   <:binding< $lid: avar_to_lid v$ = $emit_aval body$ >>
	    | Some t -> <:binding< $lid: avar_to_lid v$ : $emit_atyp t$
				   = $emit_aval body$ >> in
	<:expr< let rec $list: List.map emit_binding bindings$
		in $emit_aval body$ >>
    | Aval_if (loc, cond, cq, ccq) ->
	let _loc = p4loc loc in
	<:expr< if $emit_aval cond$ then $emit_aval cq$ else $emit_aval ccq$ >>
    | Aval_back loc ->
	errf_at loc "Backtracking else-branch is not supported here."
    | Aval_assert (loc, x, y) ->
	let _loc = p4loc loc in
	let msg = "Assertion failed." in
	<:expr<
	    begin
		if $lid: idr_to_lid idr_1o_not$ ($emit_aval x$) then
		    __failure
			(__string_of_utf8 $str: Location.to_string loc$)
			(__string_of_utf8 $str: msg$)
		else $emit_aval y$
	    end
	>>
    | Aval_trace (loc, x, y) ->
	let _loc = p4loc loc in
	let mkarg = function
	    | Aval_apply (_, Aval_apply (_, Aval_ref colon, x), Aval_ref t)
		    when apath_eq_idr idr_2o_colon colon ->
		<:expr< (__string_of_utf8 $str: Ast_print.aval_to_string x$,
			 $id: emit_apath_uid t$.show $emit_aval x$)
		>>
	    | x -> errf_at (aval_loc x) "Unsupported trace argument." in
	let xs = Ast_utils.extract_aval_o2left_idr idr_2o_comma x in
	let os = List.fold (fun x os -> <:expr< [ $mkarg x$ :: $os$ ] >>)
			   (List.rev xs) <:expr< [] >> in
	<:expr<
	    begin
		__trace (__string_of_utf8 $str: Location.to_string loc$) $os$;
		$emit_aval y$
	    end
	>>
    | Aval_raise (loc, x) ->
	let _loc = p4loc loc in
	<:expr< raise $emit_aval x$ >>
and emit_match_case (pat, ocond_opt, body) =
    let opat, ocond_opt = emit_apat pat (Option.map emit_aval ocond_opt) in
    let guard_opt, body = Ast_utils.extract_backtrack_guard body in
    let _loc = p4loc (Location.span [apat_loc pat; aval_loc body]) in
    let ocond_opt =
	match guard_opt with
	| None -> ocond_opt
	| Some guard ->
	    let oguard = emit_aval guard in
	    begin match ocond_opt with
	    | None -> Some oguard
	    | Some ocond ->
		Some <:expr< ocond && (oguard) >>
	    end in
    let obody = emit_aval body in
    match ocond_opt with
    | Some ocond ->
	<:match_case< $pat: opat$ when $ocond$ -> $obody$ >>
    | None ->
	<:match_case< $pat: opat$ -> $obody$ >>

let emit_type_binding (loc, v, params, ti) =
    let _loc = p4loc loc in
    match ti with
    | Atypinfo_abstract | Atypinfo_cabi _ ->
	List.fold (fun arg at -> <:ctyp< $at$ $emit_atyp arg$ >>) params
		  <:ctyp< $lid: avar_to_lid v$ >>
    | _ ->
    let rhs =
	match ti with
	| Atypinfo_abstract | Atypinfo_cabi _ -> assert false (* Unreachable *)
	| Atypinfo_alias typ -> emit_atyp typ
	| Atypinfo_injs injs ->
	    let emit_inj (loc, v, inj_type, _) =
		let _loc = p4loc loc in
		let rt, ats = Ast_utils.flatten_arrows inj_type in
		let ots = List.map emit_atyp ats in
		<:ctyp< $uid: avar_to_uid v$ of $list: ots$ >> in
	    <:ctyp< [ $list: List.map emit_inj injs$ ] >> in
    Ast.TyDcl (_loc, avar_to_lid v, List.map emit_atyp params, rhs, [])

let emit_curried_inj _loc inj_type inj_var =
    let locd = Location.dummy in
    let _, avars =
	Ast_utils.fold_arg_types begin fun _ (i, avs) ->
	    let name = Printf.sprintf "x%d" i in
	    (i + 1, Avar (locd, Idr name) :: avs)
	end inj_type (0, []) in
    let ov = <:expr< $uid: avar_to_uid inj_var$ >> in
    let ov = List.fold
	(fun v ov -> <:expr< $ov$ $lid: avar_to_lid v$ >>)
	(List.rev avars) ov in
    List.fold
	(fun v ov -> <:expr< fun $lid: avar_to_lid v$ -> $ov$ >>)
	avars ov

let emit_inj_aliases (loc, v, params, ti) =
    match ti with
    | Atypinfo_injs injs ->
	let emit_inj (loc, v, inj_type, _) =
	    let _loc = p4loc loc in
	    let ov = emit_curried_inj _loc inj_type v in
	    <:binding< $lid: avar_to_lid v$ = $ov$ >> in
	List.map emit_inj injs
    | Atypinfo_abstract _ | Atypinfo_alias _ | Atypinfo_cabi _ -> []

let external_names stubname t =
    if Ast_utils.arity t <= 5 then Ast.LCons (stubname, Ast.LNil) else
    Ast.LCons (stubname ^ "_byte", Ast.LCons (stubname, Ast.LNil))

type amod_state = {
    mutable ams_stub_prefix : string;
    ams_module_name : string;
}

let rec emit_asig state = function
    | Asig_ref p ->
	let _loc = p4loc (apath_loc p) in
	<:module_type< $id: emit_apath_uid p$ >>
    | Asig_decs (loc, decs) ->
	let _loc = p4loc loc in
	<:module_type< sig $list: List.map (emit_adec state) decs$ end >>
    | Asig_product (loc, xv, xsig, ysig) ->
	let _loc = p4loc loc in
	<:module_type<
	    functor ( $uid: avar_to_uid xv$ : $emit_asig state xsig$ ) ->
		$emit_asig state ysig$ >>
    | Asig_with_type (loc, s, x, y) ->
	let _loc = p4loc loc in
	let constr = <:with_constr< type $emit_atyp x$ = $emit_atyp y$ >> in
	<:module_type< $emit_asig state s$ with $constr$ >>
    | Asig_with_struct (loc, s, x, y) ->
	let _loc = p4loc loc in
	let constr = <:with_constr< module $uid: avar_to_uid x$
					 = $emit_apath_uid y$ >> in
	<:module_type< $emit_asig state s$ with $constr$ >>
and emit_adec state = function
    | Adec_include (loc, s) ->
	let _loc = p4loc loc in
	<:sig_item< include $emit_asig state s$ >>
    | Adec_open (loc, p) ->
	let _loc = p4loc loc in
	<:sig_item< open $id: emit_apath_uid p$ >>
    | Adec_use (loc, use) ->
	begin match Ast_utils.interpret_use use with
	| `Stub_prefix pfx -> state.ams_stub_prefix <- pfx
	end;
	let _loc = p4loc loc in <:sig_item< >>
    | Adec_in (loc, v, s) ->
	let _loc = p4loc loc in
	<:sig_item< module $uid: avar_to_uid v$ : $emit_asig state s$ >>
    | Adec_sig (loc, v, None) ->
	let _loc = p4loc loc in
	<:sig_item< module type $uid: avar_to_uid v$ >>
    | Adec_sig (loc, v, Some s) ->
	let _loc = p4loc loc in
	<:sig_item< module type $uid: avar_to_uid v$ = $emit_asig state s$ >>
    | Adec_types bindings ->
	let (lloc, _, _, _) = List.hd bindings in
	let (uloc, _, _, _) = List.last bindings in
	let _loc = p4loc (Location.span [lloc; uloc]) in
	<:sig_item< type $list: List.map emit_type_binding bindings$ >>
    | Adec_injx (loc, xv, xt) ->
	let _loc = p4loc loc in
	let rt, ats = Ast_utils.flatten_arrows xt in
	let ots = List.map emit_atyp ats in
	<:sig_item<
	    exception $uid: avar_to_uid xv$ of $list: ots$;
	    value $lid: avar_to_lid xv$ : $emit_atyp xt$
	>>
    | Adec_val (loc, xv, xt) ->
	let _loc = p4loc loc in
	<:sig_item< value $lid: avar_to_lid xv$ : $emit_atyp xt$ >>
    | Adec_cabi_val (loc, v, t, cn_opt, valopts)
	    when Ast_utils.atyp_is_const t ->
	let _loc = p4loc loc in
	<:sig_item< value $lid: avar_to_lid v$ : $emit_atyp t$ >>
    | Adec_cabi_val (loc, v, t, cn_opt, valopts) ->
	let _loc = p4loc loc in
	let stubname =
	    if List.mem `Is_stub valopts then
		begin match cn_opt with
		| None -> state.ams_stub_prefix ^ avar_to_lid v
		| Some cn -> cn
		end else
	    state.ams_stub_prefix ^ avar_to_lid v in
	let syms = external_names stubname t in
	let name = avar_to_lid v in
	let rt = Ast_utils.result_type t in
	if fst (Ast_utils.unwrap_atyp_action rt) <> Ast_utils.No_pocket then
	    let ot = emit_atyp_cabi_io t in
	    <:sig_item< value $lid: name$ : $ot$ >>
	else
	    let ot = emit_atyp ~typefor: Typefor_cabi t in
	    <:sig_item< external $lid: name$ : $ot$ = $str_list: syms$ >>

let rec emit_amod state = function
    | Amod_ref p ->
	let _loc = p4loc (apath_loc p) in
	<:module_expr< $id: emit_apath_uid p$ >>
    | Amod_defs (loc, defs) ->
	let _loc = p4loc loc in
	<:module_expr< struct $list: List.map (emit_adef state) defs$ end >>
    | Amod_apply (loc, x, y) ->
	let _loc = p4loc loc in
	<:module_expr< $emit_amod state x$ $emit_amod state y$ >>
    | Amod_lambda (loc, xv, xsig, ymod) ->
	let _loc = p4loc loc in
	<:module_expr<
	    functor ($uid: avar_to_uid xv$ : $emit_asig state xsig$) ->
		$emit_amod state ymod$ >>
    | Amod_coercion (loc, m, s) ->
	let _loc = p4loc loc in
	<:module_expr< ($emit_amod state m$ : $emit_asig state s$) >>
and emit_adef state = function
    | Adef_include (loc, m) ->
	let _loc = p4loc loc in
	<:str_item< include $emit_amod state m$ >>
    | Adef_open (loc, p) ->
	let _loc = p4loc loc in
	<:str_item< open $id: emit_apath_uid p$ >>
    | Adef_use (loc, use) ->
	begin match Ast_utils.interpret_use use with
	| `Stub_prefix pfx -> state.ams_stub_prefix <- pfx
	end;
	let _loc = p4loc loc in <:str_item< >>
    | Adef_in (loc, v, m) ->
	let _loc = p4loc loc in
	<:str_item< module $uid: avar_to_uid v$ = $emit_amod state m$ >>
    | Adef_sig (loc, v, s) ->
	let _loc = p4loc loc in
	<:str_item< module type $uid: avar_to_uid v$ = $emit_asig state s$ >>
    | Adef_types bindings ->
	let (lloc, _, _, _) = List.hd bindings in
	let (uloc, _, _, _) = List.last bindings in
	let _loc = p4loc (Location.span [lloc; uloc]) in
	let odef =
	    <:str_item< type $list: List.map emit_type_binding bindings$ >> in
	let alias_bindings = List.concat (List.map emit_inj_aliases bindings) in
	if alias_bindings = [] then odef else
	let odef_aliases = <:str_item< value $list: alias_bindings$ >> in
	<:str_item< $list: [odef; odef_aliases]$ >>
    | Adef_injx (loc, xv, xt) ->
	let _loc = p4loc loc in
	let rt, ats = Ast_utils.flatten_arrows xt in
	let ots = List.map emit_atyp ats in
	<:str_item<
	    exception $uid: avar_to_uid xv$ of $list: ots$;
	    value $lid: avar_to_lid xv$ = $emit_curried_inj _loc xt xv$
	>>
    | Adef_let (loc, v, x) ->
	let _loc = p4loc loc in
	let ov, ocond_opt = emit_apat v None in
	if ocond_opt <> None then
	    errf_at loc "Cannot match string literal in let-binding.";
	<:str_item< value $pat: ov$ = $emit_aval x$ >>
    | Adef_letrec bindings ->
	let (lloc, _, _, _) = List.hd bindings in
	let (uloc, _, _, _) = List.last bindings in
	let _loc = p4loc (Location.span [lloc; uloc]) in
	let emit_value_binding (loc, v, t_opt, x) =
	    let _loc = p4loc loc in
	    match t_opt with
	    | None ->
		<:binding< $lid: avar_to_lid v$ = $emit_aval x$ >>
	    | Some t ->
		<:binding< $lid: avar_to_lid v$ : $emit_atyp t$
			    = $emit_aval x$ >> in
	<:str_item< value rec $list: List.map emit_value_binding bindings$ >>
    | Adef_cabi_val (loc, v, t, cn_opt, valopts)
	    when Ast_utils.atyp_is_const t ->
	let _loc = p4loc loc in
	<:str_item< value $lid: avar_to_lid v$ : $emit_atyp t$
			= $uid: str_to_uid (state.ams_module_name ^ "_FFIC")$
			. $lid: avar_to_lid v$ >>
    | Adef_cabi_val (loc, v, t, cn_opt, valopts) ->
	let _loc = p4loc loc in
	let stubname =
	    if List.mem `Is_stub valopts then
		begin match cn_opt with
		| None -> state.ams_stub_prefix ^ avar_to_lid v
		| Some cn -> cn
		end else
	    state.ams_stub_prefix ^ avar_to_lid v in
	let syms = external_names stubname t in
	let name = avar_to_lid v in
	let ot = emit_atyp ~typefor: Typefor_cabi t in
	let rt = Ast_utils.result_type t in
	let (pocket, rt) = Ast_utils.unwrap_atyp_action rt in
	if pocket <> Ast_utils.No_pocket then
	    let cname = state.ams_stub_prefix ^ name in
	    let oxt = emit_atyp_cabi_io t in
	    let ox = <:str_item< external $lid: cname$ : $oxt$
					= $str_list: syms$ >> in
	    let r = Ast_utils.arity t in
	    let z =
		if r > 0 then
		    let mkarg i = Printf.sprintf "x%d" i in
		    let args = List.init r mkarg in
		    let y = List.fold (fun x f -> <:expr< $f$ $lid: x$ >>) args
				      <:expr< $lid: cname$ >> in
		    let y = <:expr< __unsafe_action (fun () -> $y$) >> in
		    List.fold (fun x y -> <:expr< fun $lid: x$ -> $y$ >>)
			      (List.rev args) y
		else
		    <:expr< __unsafe_action $lid: cname$ >> in
	    let ov = <:str_item< value $lid: name$ : $ot$ = $z$ >> in
	    <:str_item< $list: [ox; ov]$ >>
	else
	    <:str_item< external $lid: name$ : $ot$ = $str_list: syms$ >>
    | Adef_cabi_open (loc, _) ->
	let _loc = p4loc loc in <:str_item< >>

let emit_toplevel ~module_name = function
    | Amod_defs (loc, defs) ->
	let state = {
	    ams_stub_prefix = "_stub_";
	    ams_module_name = module_name;
	} in
	<:str_item< $list: List.map (emit_adef state) defs$ >>
    | amod ->
	errf_at (amod_loc amod) "Module expression not allowed at top-level."
