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

type emit_context = {
    ec_module_name : string;
    ec_modpath : Modpath.t;
    ec_utvars : Idr_set.t;
    mutable ec_stub_prefix : string;
}

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

let emit_apath_lid (Apath (loc, p)) =
    let _loc = p4loc loc in
    let ci = <:ident< $lid: idr_to_lid (Modpath.last_e p)$ >> in
    let cns = Modpath.to_idr_list (Modpath.strip_last_e p) in
    let cis = List.map (fun cn -> <:ident< $uid: idr_to_uid cn$ >>) cns in
    <:ident< $list: cis @ [ci]$ >>
let emit_apath_uid (Apath (loc, p)) =
    let _loc = p4loc loc in
    let cns = Modpath.to_idr_list p in
    let cis = List.map (fun cn -> <:ident< $uid: idr_to_uid cn$ >>) cns in
    <:ident< $list: cis$ >>

type typefor = Typefor_viz | Typefor_cabi | Typefor_cabi_io | Typefor_method

let rec emit_atyp ec ?(typefor = Typefor_viz) = function
    | Atyp_uvar v ->
	let _loc = p4loc (avar_loc v) in
	<:ctyp< '$lid: avar_to_lid v$ >>
    | Atyp_ref p ->
	let _loc = p4loc (apath_loc p) in
	<:ctyp< $id: emit_apath_lid p$ >>
    | Atyp_A (loc, v, t) ->
	let _loc = p4loc loc in
	if typefor = Typefor_method then
	    <:ctyp< ! '$lid: avar_to_lid v$. $emit_atyp ec t$ >> else
	<:ctyp< < __it : ! '$lid: avar_to_lid v$. $emit_atyp ec ~typefor t$ > >>
    | Atyp_E (loc, v, t) ->
	warnf_at loc "Ignoring quantification of %s." (avar_name v);
	emit_atyp ec ~typefor t
    | Atyp_apply (loc, Atyp_apply (_, Atyp_ref p_times, t), u)
	    when apath_eq_idr Cst_core.idr_2o_times p_times ->
	let _loc = p4loc loc in
	<:ctyp< ($emit_atyp ec t$ * $emit_atyp ec u$) >>
    | Atyp_apply (loc, Atyp_apply (_, Atyp_ref p_index, t), u)
	    when (typefor = Typefor_cabi || typefor = Typefor_cabi_io)
	      && apath_eq_idr Cst_core.idr_2o_index p_index ->
	emit_atyp ec ~typefor t
    | Atyp_apply (loc, at, au) ->
	if typefor = Typefor_cabi_io
		&& Ast_utils.atyp_effect_pocket at <> Ast_utils.No_pocket then
	    emit_atyp ec ~typefor au
	else
	    let _loc = p4loc loc in
	    <:ctyp< $emit_atyp ec ~typefor at$ $emit_atyp ec ~typefor au$ >>
    | Atyp_arrow (loc, Alabel_none, at, au) ->
	let _loc = p4loc loc in
	<:ctyp< $emit_atyp ec ~typefor at$ -> $emit_atyp ec ~typefor au$ >>
    | Atyp_arrow (loc, Alabel_labelled label, at, au) ->
	let _loc = p4loc loc in
	<:ctyp< $lid: idr_to_lid label$: $emit_atyp ec ~typefor at$ ->
	    $emit_atyp ec ~typefor au$ >>
    | Atyp_arrow (loc, Alabel_optional label, at, au) ->
	let _loc = p4loc loc in
	<:ctyp< ? $lid: idr_to_lid label$: $emit_atyp ec ~typefor at$ ->
	    $emit_atyp ec ~typefor au$ >>

let emit_atyp_cabi_io ec = function
    | Atyp_arrow _ as t ->
	emit_atyp ec ~typefor: Typefor_cabi_io t
    | t ->
	let _loc = p4loc (atyp_loc t) in
	<:ctyp< unit -> $emit_atyp ec ~typefor: Typefor_cabi_io t$ >>

let emit_apat_literal loc lit ocond_opt =
    let _loc = p4loc loc in
    match lit with
    | Lit_unit     -> <:patt< () >>, ocond_opt
    | Lit_bool x   -> (if x then <:patt<True>> else <:patt<False>>), ocond_opt
    | Lit_int x    -> let s = string_of_int x in <:patt< $int:s$ >>, ocond_opt
    | Lit_float x  -> let s = string_of_float x in <:patt<$flo:s$>>, ocond_opt
    | Lit_char x   ->
	let v = fresh_avar_at loc in
	let cond = <:expr< __generic_eq (__char_code $lid: avar_to_lid v$)
				$int: string_of_int (UChar.code x)$ >> in
	let cond =
	    match ocond_opt with
	    | None -> cond
	    | Some cond' -> <:expr< $cond'$ && $cond$ >> in
	<:patt< $lid: avar_to_lid v$ >>, Some cond
    | Lit_string x ->
	let v = fresh_avar_at loc in
	let xstr = String.escaped (UString.to_utf8 x) in
	let cond = <:expr< __generic_eq $lid: avar_to_lid v$
					(__string_of_utf8 $str: xstr$) >> in
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
    | Lit_char x ->
	<:expr< __char_of_code $int: string_of_int (UChar.code x)$ >>
    | Lit_string x ->
	<:expr< __string_of_utf8 $str: String.escaped (UString.to_utf8 x)$ >>

let emit_apat_fixed _loc default = function
    | "()" | "0'(')" -> <:patt< () >>
    | "[]" | "0'[']" -> <:patt< [] >>
    | "#[]" | "0'#[']" -> <:patt< [| |] >>
    | _ -> default ()

let rec emit_apat ec = function
    | Apat_literal (loc, lit) -> fun ocond_opt utvars ->
	let opat, ocond_opt = emit_apat_literal loc lit ocond_opt in
	(opat, ocond_opt, utvars)
    | Apat_ref p -> fun ocond_opt utvars ->
	let _loc = p4loc (apath_loc p) in
	let default () = <:patt< $id: emit_apath_uid p$ >> in
	begin match p with
	| Apath (_, p) when Modpath.is_atom p ->
	    emit_apat_fixed _loc default (idr_to_string (Modpath.last_e p))
	| _ -> default ()
	end, ocond_opt, utvars
    | Apat_uvar v ->
	let _loc = p4loc (avar_loc v) in
	fun ocond_opt utvars ->
	<:patt< $lid: avar_to_lid v$ >>, ocond_opt, utvars
    | Apat_apply (loc, Alabel_none,
	Apat_apply (_, Alabel_none, Apat_ref op, x), y)
	    when apath_eq_idr Cst_core.idr_list_push op ->
	fun ocond_opt utvars ->
	let _loc = p4loc loc in
	let ox, ocond_opt, utvars = emit_apat ec x ocond_opt utvars in
	let oy, ocond_opt, utvars = emit_apat ec y ocond_opt utvars in
	<:patt< [$ox$ :: $oy$] >>, ocond_opt, utvars
    | Apat_apply (loc, Alabel_none,
	Apat_apply (_, Alabel_none, Apat_ref op, x), y)
	    when apath_eq_idr Cst_core.idr_2o_comma op ->
	fun ocond_opt utvars ->
	let _loc = p4loc loc in
	let ox, ocond_opt, utvars = emit_apat ec x ocond_opt utvars in
	let oy, ocond_opt, utvars = emit_apat ec y ocond_opt utvars in
	<:patt< ($ox$, $oy$) >>, ocond_opt, utvars
    | Apat_apply (loc, Alabel_none, x, y) -> fun ocond_opt utvars ->
	let _loc = p4loc loc in
	let ox, ocond_opt, utvars = emit_apat ec x ocond_opt utvars in
	let oy, ocond_opt, utvars = emit_apat ec y ocond_opt utvars in
	<:patt< $ox$ $oy$ >>, ocond_opt, utvars
    | Apat_apply (loc, _, _, _) -> fun ocond_opt utvars ->
	errf_at loc "Label not allowed here."
    | Apat_as (loc, v, x) -> fun ocond_opt utvars ->
	let _loc = p4loc loc in
	let ox, ocond_opt, utvars = emit_apat ec x ocond_opt utvars in
	<:patt< ($ox$ as $lid: avar_to_lid v$) >>, ocond_opt, utvars
    | Apat_intype (loc, t, x) -> fun ocond_opt utvars ->
	let _loc = p4loc loc in
	let ox, ocond_opt, utvars = emit_apat ec x ocond_opt utvars in
	let utvars =
	    match t, x with
	    | Atyp_A _, Apat_uvar (Avar (_, idr)) -> Idr_set.add idr utvars
	    | _ -> utvars in
	<:patt< ($ox$ : $emit_atyp ec t$) >>, ocond_opt, utvars

let emit_aval_fixed _loc default = function
    | "()" | "0'(')" -> <:expr< () >>
    | "[]" | "0'[']" -> <:expr< [] >>
    | "#[]" | "0'#[']" -> <:expr< [| |] >>
    | "[;]" -> <:expr< Data.List.push >>
    | _ -> default ()

let apat_matches_vacuous = function
    | Apat_apply (_, Alabel_none, Apat_ref qmark, _) ->
	apath_eq_idr idr_1o_qmark qmark
    | _ -> false

let rec emit_aval ec = function
    | Aval_literal (loc, lit) -> emit_aval_literal loc lit
    | Aval_ref p ->
	let _loc = p4loc (apath_loc p) in
	let default () = <:expr< $id: emit_apath_lid p$ >> in
	begin match p with
	| Apath (_, p) when Modpath.is_atom p ->
	    let v = Modpath.last_e p in
	    if Idr_set.mem v ec.ec_utvars then
		<:expr< $lid: idr_to_string v$ # __it >> else
	    emit_aval_fixed _loc default (idr_to_string v)
	| _ -> default ()
	end
    | Aval_apply (loc, Alabel_none,
	Aval_apply (_, Alabel_none, Aval_ref op, x), y)
	    when apath_eq_idr Cst_core.idr_list_push op ->
	let _loc = p4loc loc in
	<:expr< [$emit_aval ec x$ :: $emit_aval ec y$] >>
    | Aval_apply (loc, Alabel_none,
	Aval_apply (_, Alabel_none, Aval_ref op, x), y)
	    when apath_eq_idr Cst_core.idr_2o_comma op ->
	let _loc = p4loc loc in
	<:expr< ($emit_aval ec x$, $emit_aval ec y$) >>
    | Aval_apply (loc, Alabel_none, x, y) ->
	let _loc = p4loc loc in
	<:expr< $emit_aval ec x$ $emit_aval ec y$ >>
    | Aval_apply (loc, Alabel_labelled l, x, y) ->
	let _loc = p4loc loc in
	<:expr< $emit_aval ec x$ ~ $lid: idr_to_lid l$: $emit_aval ec y$ >>
    | Aval_apply (loc, Alabel_optional l, x, y) ->
	let _loc = p4loc loc in
	<:expr< $emit_aval ec x$ ? $lid: idr_to_lid l$: $emit_aval ec y$ >>
    | Aval_array (loc, xs) ->
	let _loc = p4loc loc in
	<:expr< [| $list: List.map (emit_aval ec) xs$ |] >>
    | Aval_at (loc, None, cases) ->
	let _loc = p4loc loc in
	<:expr< fun [ $list: List.map (emit_match_case ec) cases$ ] >>
    | Aval_at (loc, Some l, cases) ->
	let _loc = p4loc loc in
	let ol = idr_to_lid l in
	let have_vacuous =
	    List.exists (fun (apat, _, _) -> apat_matches_vacuous apat) cases in
	let ocases = List.map (emit_match_case ~have_vacuous ec) cases in
	if have_vacuous then
	    <:expr< fun ? $lid: ol$ ->
		    match $lid: ol$ with [ $list: ocases$ ] >>
	else
	    <:expr< fun ~ $lid: ol$ ->
		    match $lid: ol$ with [ $list: ocases$ ] >>
    | Aval_match (loc, x, cases) ->
	let _loc = p4loc loc in
	<:expr< match $emit_aval ec x$
		with [ $list: List.map (emit_match_case ec) cases$ ] >>
    | Aval_let (loc, p, x, body) ->
	let _loc = p4loc loc in
	let op, ocond_opt, utvars = emit_apat ec p None ec.ec_utvars in
	if ocond_opt <> None then
	    errf_at loc "Cannot match string literals in let-binding.";
	let ec = {ec with ec_utvars = utvars } in
	<:expr< let $pat: op$ = $emit_aval ec x$ in $emit_aval ec body$ >>
    | Aval_letrec (loc, bindings, body) ->
	let _loc = p4loc loc in
	let emit_binding (loc, v, topt, body) =
	    let _loc = p4loc loc in
	    match topt with
	    | None ->   <:binding< $lid: avar_to_lid v$ = $emit_aval ec body$ >>
	    | Some t -> <:binding< $lid: avar_to_lid v$ : $emit_atyp ec t$
				   = $emit_aval ec body$ >> in
	<:expr< let rec $list: List.map emit_binding bindings$
		in $emit_aval ec body$ >>
    | Aval_if (loc, cond, cq, ccq) ->
	let _loc = p4loc loc in
	<:expr< if $emit_aval ec cond$ then $emit_aval ec cq$ else
		$emit_aval ec ccq$ >>
    | Aval_back loc ->
	errf_at loc "Backtracking else-branch is not supported here."
    | Aval_seq (loc, op, x, y) when op = Idr "assert" ->
	let _loc = p4loc loc in
	let msg = "Assertion failed." in
	let xloc = aval_loc x in
	<:expr<
	    begin
		if $lid: idr_to_lid idr_1o_not$ ($emit_aval ec x$) then
		    __failure
			(__string_of_utf8 $str: Location.to_string xloc$)
			(__string_of_utf8 $str: msg$)
		else $emit_aval_opt ec loc y$
	    end
	>>
    | Aval_seq (loc, op, x, y) when op = Idr "__trace" ->
	let _loc = p4loc loc in
	let mkarg = function
	    | Aval_intype (_, Atyp_ref t, x) ->
		<:expr< (__string_of_utf8 $str: Ast_print.aval_to_string x$,
			 $id: emit_apath_uid t$.show $emit_aval ec x$)
		>>
	    | x -> errf_at (aval_loc x) "Unsupported trace argument." in
	let xs = Ast_utils.extract_aval_o2left_idr idr_2o_comma x in
	let os = List.fold (fun x os -> <:expr< [ $mkarg x$ :: $os$ ] >>)
			   (List.rev xs) <:expr< [] >> in
	<:expr<
	    begin
		__trace (__string_of_utf8 $str: Location.to_string loc$) $os$;
		$emit_aval_opt ec loc y$
	    end
	>>
    | Aval_seq (loc, op, _, _) ->
	errf_at loc "Unsupported sequencing verb %s." (idr_to_string op)
    | Aval_raise (loc, x) ->
	let _loc = p4loc loc in
	<:expr< __builtin_raise $emit_aval ec x$ >>
    | Aval_intype (loc, t, x) ->
	let _loc = p4loc loc in
	let ox = emit_aval ec x in
	begin match t with
	| Atyp_A _ ->
	    let ot = emit_atyp ~typefor:Typefor_method ec t in
	    <:expr< (object method __it : $ot$ = $ox$; end) >>
	| _ ->
	    <:expr< ($ox$ : $emit_atyp ec t$) >>
	end
and emit_aval_opt ec loc = function
    | None -> let _loc = p4loc loc in <:expr< () >>
    | Some cx -> emit_aval ec cx
and emit_match_case ?(have_vacuous = false) ec (pat, ocond_opt, body) =
    let _loc = p4loc (Location.span [apat_loc pat; aval_loc body]) in
    if apat_matches_vacuous pat then begin
	Option.iter
	    (fun cond ->
		errf_at (aval_loc cond)
			"Condition not allowed on default pattern.") ocond_opt;
	match pat with
	| Apat_apply (_, Alabel_none, Apat_ref qmark, pat)
		when apath_eq_idr idr_1o_qmark qmark ->
	    begin match emit_apat ec pat None ec.ec_utvars with
	    | opat, None, utvars ->
		let ec = {ec with ec_utvars = utvars} in
		let obody = emit_aval ec body in
		<:match_case< $pat: opat$ -> $obody$ >>
	    | _ -> errf_at (apat_loc pat) "A variable is expected before '?'."
	    end
	| _ -> errf_at (apat_loc pat) "Invalid pattern."
    end else
    let opat, ocond_opt, utvars =
	emit_apat ec pat (Option.map (emit_aval ec) ocond_opt) ec.ec_utvars in
    let ec = {ec with ec_utvars = utvars} in
    let opat = if have_vacuous then <:patt< Some $opat$ >> else opat in
    let guard_opt, body = Ast_utils.extract_backtrack_guard body in
    let ocond_opt =
	match guard_opt with
	| None -> ocond_opt
	| Some guard ->
	    let oguard = emit_aval ec guard in
	    begin match ocond_opt with
	    | None -> Some oguard
	    | Some ocond ->
		Some <:expr< ocond && (oguard) >>
	    end in
    let obody = emit_aval ec body in
    match ocond_opt with
    | Some ocond ->
	<:match_case< $pat: opat$ when $ocond$ -> $obody$ >>
    | None ->
	<:match_case< $pat: opat$ -> $obody$ >>

let emit_type_binding ec (loc, v, params, ti) =
    let _loc = p4loc loc in
    match ti with
    | Atypinfo_abstract | Atypinfo_cabi _ ->
	List.fold (fun arg at -> <:ctyp< $at$ $emit_atyp ec arg$ >>) params
		  <:ctyp< $lid: avar_to_lid v$ >>
    | _ ->
    let rhs =
	match ti with
	| Atypinfo_abstract | Atypinfo_cabi _ -> assert false (* Unreachable *)
	| Atypinfo_alias typ -> emit_atyp ec typ
	| Atypinfo_injs injs ->
	    let emit_inj (loc, v, inj_type, _) =
		let _loc = p4loc loc in
		let rt, ats = Ast_utils.flatten_arrows inj_type in
		let ots = List.map (emit_atyp ec) ats in
		<:ctyp< $uid: avar_to_uid v$ of $list: ots$ >> in
	    <:ctyp< [ $list: List.map emit_inj injs$ ] >> in
    Ast.TyDcl (_loc, avar_to_lid v, List.map (emit_atyp ec) params, rhs, [])

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

let emit_inj_alias_decs ec (loc, v, params, ti) =
    match ti with
    | Atypinfo_injs injs ->
	let emit_inj_dec (loc, v, inj_type, _) =
	    let _loc = p4loc loc in
	    <:sig_item< value $lid: avar_to_lid v$ :
			      $emit_atyp ec inj_type$ >> in
	List.map emit_inj_dec injs
    | Atypinfo_abstract _ | Atypinfo_alias _ | Atypinfo_cabi _ -> []

let external_names stubname t =
    if Ast_utils.arity t <= 5 then Ast.LCons (stubname, Ast.LNil) else
    Ast.LCons (stubname ^ "_byte", Ast.LCons (stubname, Ast.LNil))

let rec emit_asig ec = function
    | Asig_ref p ->
	let _loc = p4loc (apath_loc p) in
	<:module_type< $id: emit_apath_uid p$ >>
    | Asig_decs (loc, decs) ->
	let _loc = p4loc loc in
	<:module_type< sig $list: List.map (emit_adec ec) decs$ end >>
    | Asig_product (loc, xv, xsig, ysig) ->
	let _loc = p4loc loc in
	<:module_type<
	    functor ($uid: avar_to_uid xv$ : $emit_asig ec xsig$) ->
		$emit_asig ec ysig$ >>
    | Asig_suspension (loc, xsig) ->
	let _loc = p4loc loc in
	let dec = <:sig_item< type suspended_ >> in
	<:module_type<
	    functor (Suspended_ : sig $list: [dec]$ end) ->
		$emit_asig ec xsig$ >>
    | Asig_with_type (loc, s, x, y) ->
	let _loc = p4loc loc in
	let constr =
	    <:with_constr< type $emit_atyp ec x$ = $emit_atyp ec y$ >> in
	<:module_type< $emit_asig ec s$ with $constr$ >>
    | Asig_with_struct (loc, s, x, y) ->
	let _loc = p4loc loc in
	let constr = <:with_constr< module $uid: avar_to_uid x$
					 = $emit_apath_uid y$ >> in
	<:module_type< $emit_asig ec s$ with $constr$ >>
and emit_adec ec = function
    | Adec_include (loc, s) ->
	let _loc = p4loc loc in
	<:sig_item< include $emit_asig ec s$ >>
    | Adec_open (loc, (Apath (_, p) as ap)) ->
	let _loc = p4loc loc in
	<:sig_item< open $id: emit_apath_uid ap$ >>
    | Adec_use (loc, use) ->
	begin match Ast_utils.interpret_use use with
	| `Stub_prefix pfx -> ec.ec_stub_prefix <- pfx
	| `type_c _ -> ()
	end;
	let _loc = p4loc loc in <:sig_item< >>
    | Adec_in (loc, v, s) ->
	let _loc = p4loc loc in
	let vn = avar_name v in
	let ec' = {ec with
	    ec_stub_prefix = ec.ec_stub_prefix ^ "_" ^ vn ^ "_";
	    ec_modpath = Modpath.cat_last (Idr vn) ec.ec_modpath;
	} in
	<:sig_item< module $uid: avar_to_uid v$ : $emit_asig ec' s$ >>
    | Adec_sig (loc, v, None) ->
	let _loc = p4loc loc in
	<:sig_item< module type $uid: avar_to_uid v$ >>
    | Adec_sig (loc, v, Some s) ->
	let _loc = p4loc loc in
	<:sig_item< module type $uid: avar_to_uid v$ = $emit_asig ec s$ >>
    | Adec_types bindings ->
	let (lloc, _, _, _) = List.hd bindings in
	let (uloc, _, _, _) = List.last bindings in
	let _loc = p4loc (Location.span [lloc; uloc]) in
	let otdec =
	    <:sig_item< type $list: List.map (emit_type_binding ec)
					     bindings$ >> in
	let alias_bindings =
	    List.concat (List.map (emit_inj_alias_decs ec) bindings) in
	<:sig_item< $list: otdec :: alias_bindings$ >>
    | Adec_injx (loc, xv, xt) ->
	let _loc = p4loc loc in
	let rt, ats = Ast_utils.flatten_arrows xt in
	let ots = List.map (emit_atyp ec) ats in
	<:sig_item<
	    exception $uid: avar_to_uid xv$ of $list: ots$;
	    value $lid: avar_to_lid xv$ : $emit_atyp ec xt$
	>>
    | Adec_val (loc, xv, xt) ->
	let _loc = p4loc loc in
	<:sig_item< value $lid: avar_to_lid xv$ : $emit_atyp ec xt$ >>
    | Adec_cabi_val (loc, v, t, cn_opt, valopts)
	    when Ast_utils.atyp_is_const t ->
	let _loc = p4loc loc in
	<:sig_item< value $lid: avar_to_lid v$ : $emit_atyp ec t$ >>
    | Adec_cabi_val (loc, v, t, cn_opt, valopts) ->
	let _loc = p4loc loc in
	let stubname =
	    if List.mem `Is_stub valopts then
		begin match cn_opt with
		| None -> ec.ec_stub_prefix ^ avar_to_lid v
		| Some cn -> cn
		end else
	    ec.ec_stub_prefix ^ avar_to_lid v in
	let syms = external_names stubname t in
	let name = avar_to_lid v in
	let rt = Ast_utils.result_type t in
	if fst (Ast_utils.unwrap_atyp_effect rt) <> Ast_utils.No_pocket then
	    let ot = emit_atyp_cabi_io ec t in
	    <:sig_item< value $lid: name$ : $ot$ >>
	else
	    let ot = emit_atyp ec ~typefor: Typefor_cabi t in
	    <:sig_item< external $lid: name$ : $ot$ = $str_list: syms$ >>

let rec emit_amod ec = function
    | Amod_ref p ->
	let _loc = p4loc (apath_loc p) in
	<:module_expr< $id: emit_apath_uid p$ >>
    | Amod_defs (loc, defs) ->
	let _loc = p4loc loc in
	<:module_expr< struct $list: List.map (emit_adef ec) defs$ end >>
    | Amod_apply (loc, x, y) ->
	let _loc = p4loc loc in
	<:module_expr< $emit_amod ec x$ $emit_amod ec y$ >>
    | Amod_lambda (loc, xv, xsig, ymod) ->
	let _loc = p4loc loc in
	<:module_expr<
	    functor ($uid: avar_to_uid xv$ : $emit_asig ec xsig$) ->
		$emit_amod ec ymod$ >>
    | Amod_suspend (loc, xmod) ->
	let _loc = p4loc loc in
	let dec = <:sig_item< type suspended_ >> in
	<:module_expr<
	    functor (Suspended_ : sig $list: [dec]$ end) ->
		$emit_amod ec xmod$ >>
    | Amod_generate (loc, xmod) ->
	let _loc = p4loc loc in
	let def = <:str_item< type suspended_ = unit >> in
	<:module_expr< $emit_amod ec xmod$ (struct $list: [def]$ end) >>
    | Amod_coercion (loc, m, s) ->
	let _loc = p4loc loc in
	<:module_expr< ($emit_amod ec m$ : $emit_asig ec s$) >>
and emit_adef ec = function
    | Adef_include (loc, m) ->
	let _loc = p4loc loc in
	<:str_item< include $emit_amod ec m$ >>
    | Adef_open (loc, (Apath (_, p) as ap)) ->
	let _loc = p4loc loc in
	<:str_item< open $id: emit_apath_uid ap$ >>
    | Adef_use (loc, use) ->
	begin match Ast_utils.interpret_use use with
	| `Stub_prefix pfx -> ec.ec_stub_prefix <- pfx
	| `type_c _ -> ()
	end;
	let _loc = p4loc loc in <:str_item< >>
    | Adef_in (loc, v, m) ->
	let _loc = p4loc loc in
	let vn = avar_name v in
	let ec' = {ec with
	    ec_stub_prefix = ec.ec_stub_prefix ^ "_" ^ vn ^ "_";
	    ec_modpath = Modpath.cat_last (Idr vn) ec.ec_modpath;
	} in
	<:str_item< module $uid: avar_to_uid v$ = $emit_amod ec' m$ >>
    | Adef_sig (loc, v, s) ->
	let _loc = p4loc loc in
	<:str_item< module type $uid: avar_to_uid v$ = $emit_asig ec s$ >>
    | Adef_types bindings ->
	let (lloc, _, _, _) = List.hd bindings in
	let (uloc, _, _, _) = List.last bindings in
	let _loc = p4loc (Location.span [lloc; uloc]) in
	let odef =
	    <:str_item< type $list: List.map (emit_type_binding ec)
					     bindings$ >> in
	let alias_bindings = List.concat (List.map emit_inj_aliases bindings) in
	if alias_bindings = [] then odef else
	let odef_aliases = <:str_item< value $list: alias_bindings$ >> in
	<:str_item< $list: [odef; odef_aliases]$ >>
    | Adef_injx (loc, xv, xt) ->
	let _loc = p4loc loc in
	let rt, ats = Ast_utils.flatten_arrows xt in
	let ots = List.map (emit_atyp ec) ats in
	<:str_item<
	    exception $uid: avar_to_uid xv$ of $list: ots$;
	    value $lid: avar_to_lid xv$ = $emit_curried_inj _loc xt xv$
	>>
    | Adef_let (loc, v, x) ->
	let _loc = p4loc loc in
	let ov, ocond_opt, utvars = emit_apat ec v None ec.ec_utvars in
	let ec = {ec with ec_utvars = utvars} in
	if ocond_opt <> None then
	    errf_at loc "Cannot match string literal in let-binding.";
	<:str_item< value $pat: ov$ = $emit_aval ec x$ >>
    | Adef_letrec bindings ->
	let (lloc, _, _, _) = List.hd bindings in
	let (uloc, _, _, _) = List.last bindings in
	let _loc = p4loc (Location.span [lloc; uloc]) in
	let emit_value_binding (loc, v, t_opt, x) =
	    let _loc = p4loc loc in
	    match t_opt with
	    | None ->
		<:binding< $lid: avar_to_lid v$ = $emit_aval ec x$ >>
	    | Some t ->
		<:binding< $lid: avar_to_lid v$ : $emit_atyp ec t$
			    = $emit_aval ec x$ >> in
	<:str_item< value rec $list: List.map emit_value_binding bindings$ >>
    | Adef_cabi_val (loc, v, t, cn_opt, valopts)
	    when Ast_utils.atyp_is_const t ->
	let _loc = p4loc loc in
	<:str_item< value $lid: avar_to_lid v$ : $emit_atyp ec t$
			= $uid: str_to_uid (ec.ec_module_name ^ "_FFIC")$
			. $lid: avar_to_lid v$ >>
    | Adef_cabi_val (loc, v, t, cn_opt, valopts) ->
	let _loc = p4loc loc in
	let stubname =
	    if List.mem `Is_stub valopts then
		begin match cn_opt with
		| None -> ec.ec_stub_prefix ^ avar_to_lid v
		| Some cn -> cn
		end else
	    ec.ec_stub_prefix ^ avar_to_lid v in
	let syms = external_names stubname t in
	let name = avar_to_lid v in
	let ot = emit_atyp ec ~typefor: Typefor_cabi t in
	let rt = Ast_utils.result_type t in
	let (pocket, rt) = Ast_utils.unwrap_atyp_effect rt in
	if pocket <> Ast_utils.No_pocket then
	    let cname = ec.ec_stub_prefix ^ name in
	    let oxt = emit_atyp_cabi_io ec t in
	    let ox = <:str_item< external $lid: cname$ : $oxt$
					= $str_list: syms$ >> in
	    let r = Ast_utils.arity t in
	    let z =
		if r > 0 then
		    let mkarg i = Printf.sprintf "x%d" i in
		    let args = List.init r mkarg in
		    let y = List.fold (fun x f -> <:expr< $f$ $lid: x$ >>) args
				      <:expr< $lid: cname$ >> in
		    let y = <:expr< { __unsafe_thunk = (fun () -> $y$) } >> in
		    List.fold (fun x y -> <:expr< fun $lid: x$ -> $y$ >>)
			      (List.rev args) y
		else
		    <:expr< { __unsafe_thunk = $lid: cname$ } >> in
	    let ov = <:str_item< value $lid: name$ : $ot$ = $z$ >> in
	    <:str_item< $list: [ox; ov]$ >>
	else
	    <:str_item< external $lid: name$ : $ot$ = $str_list: syms$ >>
    | Adef_cabi_open (loc, _) ->
	let _loc = p4loc loc in <:str_item< >>

let emit_toplevel ~modpath = function
    | Amod_defs (loc, defs) ->
	let ec = {
	    ec_stub_prefix = "_cviz_";
	    ec_module_name = idr_to_string (Modpath.last_e modpath);
	    ec_modpath = modpath;
	    ec_utvars = Idr_set.empty;
	} in
	<:str_item< $list: List.map (emit_adef ec) defs$ >>
    | amod ->
	errf_at (amod_loc amod) "Module expression not allowed at top-level."
