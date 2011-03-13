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
open Cst_core
open Ast_types
open Ast_core
open Leaf_types
open Leaf_core
open Diag
open FfPervasives
open Unicode

let cidr_to_avar (Cidr (loc, idr)) = Avar (loc, idr)

let apath_to_avar = function
    | Apath ([], av) -> av
    | ap -> errf_at (apath_loc ap) "Expecting an unqualified name."

let build_avar = function
    | Ctrm_ref (cidr, idrhint) -> cidr_to_avar cidr
    | ctrm -> errf_at (ctrm_loc ctrm) "Expecting an identifier."

let build_apath ctrm =
    let rec loop avs = function
	| Ctrm_project (loc, cidr, ctrm) ->
	    loop (cidr_to_avar cidr :: avs) ctrm
	| Ctrm_ref (cidr, hint) ->
	    List.rev (cidr_to_avar cidr :: avs)
	| ct -> errf_at (ctrm_loc ct) "Expecting a variable or path." in
    match loop [] ctrm with
    | av :: avs -> Apath (avs, av)
    | _ -> assert false (* unreachable *)

let rec build_atyp ?(strip_indices = false) = function
    | Ctrm_ref (cidr, Ih_univ) ->
	Atyp_uvar (cidr_to_avar cidr)
    | Ctrm_ref (cidr, _) ->
	Atyp_ref (Apath ([], cidr_to_avar cidr))
    | Ctrm_project _ as ctrm ->
	Atyp_ref (build_apath ctrm)
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (op, _), ct), cu)
	    when cidr_is_2o_arrow op ->
	let at = build_atyp ~strip_indices ct in
	let au = build_atyp ~strip_indices cu in
	Atyp_arrow (loc, at, au)
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (op, _), ct), cu)
	    when strip_indices && cidr_is_2o_index op ->
	build_atyp ~strip_indices ct
    | Ctrm_apply (loc, ct, cu) ->
	let at = build_atyp ~strip_indices ct in
	let au = build_atyp ~strip_indices cu in
	Atyp_apply (loc, at, au)
    | ct -> errf_at (ctrm_loc ct) "Invalid type expression."

let build_atyp_con_args =
    let rec loop args = function
	| Ctrm_ref (Cidr (loc, idr), _) -> (Avar (loc, idr), args)
	| Ctrm_apply (loc, ct, arg) -> loop (build_atyp arg :: args) ct
	| ct -> errf_at (ctrm_loc ct) "Expecting a type constructor." in
    loop []

let rec build_apat ?(adecmap = Idr_map.empty) ?(fpos = false) = function
    | Ctrm_literal (loc, lit) ->
	Apat_literal (loc, lit)
    | Ctrm_ref (cidr, Ih_inj) ->
	Apat_ref (Apath ([], cidr_to_avar cidr))
    | Ctrm_ref (cidr, _) ->
	if fpos then Apat_ref (Apath ([], cidr_to_avar cidr)) else
	let apat = Apat_uvar (cidr_to_avar cidr) in
	begin try
	    let (loc, t) = Idr_map.find (cidr_to_idr cidr) adecmap in
	    Apat_intype (loc, t, apat)
	with Not_found -> apat end
    | Ctrm_project _ as ctrm ->
	Apat_ref (build_apath ctrm)
    | Ctrm_apply (loc, cx, cy) ->
	let ax = build_apat ~adecmap ~fpos:true cx in
	let ay = build_apat ~adecmap cy in
	Apat_apply (loc, ax, ay)
    | cx -> errf_at (ctrm_loc cx) "Invalid pattern."

let make_aval_return loc decor ax =
    let af = aval_ref_of_idr loc (Idr ("return" ^ decor)) in
    Aval_apply (loc, af, ax)
let make_aval_bind loc decor ax ay =
    let af = aval_ref_of_idr loc (Idr ("2'>>=" ^ decor)) in
    Aval_apply (loc, Aval_apply (loc, af, ax), ay)
let make_aval_chop loc decor ax ay =
    let af = aval_ref_of_idr loc (Idr ("2'>>" ^ decor)) in
    Aval_apply (loc, Aval_apply (loc, af, ax), ay)
let idr_for_line loc =
    let lineno = Location.Bound.lineno (Location.lbound loc) in
    Idr (Printf.sprintf "_m%d" lineno)
let avar_for_line loc = Avar (loc, idr_for_line loc)

type monad_mode = MM_quote of cmonad | MM_bind of aval

let wrap_abstractions cpat arhs =
    let wrap carg arhs =
	let aarg = build_apat carg in
	Aval_at (ctrm_loc carg, [(aarg, None, arhs)]) in
    let cpat, arhs = Cst_utils.fold_formal_args wrap (cpat, arhs) in
    (build_apat cpat, arhs)

let rec build_aval cm_opt cpred =
    match cm_opt with
    | Some cm -> build_aval_monad (MM_quote cm) cpred
    | None ->    build_aval_pure cpred
and build_aval_pure = function
    | Cpred_let (loc, cm_opt, cpat, cpred, ccont)	(* Pattern Case *)
	    when not (Cst_utils.is_formal cpat) ->
	let arhs = build_aval cm_opt cpred in
	let apat = build_apat cpat in
	let acont = build_aval_pure ccont in
	Aval_let (loc, apat, arhs, acont)
    | Cpred_let (loc, _, _, _, _) as clet ->		(* Recursive Case *)
	let rec loop bindings = function
	    | Cpred_let (loc, cm_opt, cpat, crhs, ccont)
		    when Cst_utils.is_formal cpat ->
		let arhs = build_aval cm_opt crhs in
		let apat, arhs = wrap_abstractions cpat arhs in
		loop ((loc, apat, arhs) :: bindings) ccont
	    | ccont ->
		let acont = build_aval_pure ccont in
		Aval_letrec (loc, List.rev bindings, acont) in
	loop [] clet
    | Cpred_if (loc, cond, cq, ccq) ->
	Aval_if (loc, build_aval_expr cond, build_aval_pure cq, build_aval_pure ccq)
    | Cpred_at (loc, cases) ->
	let build_case (cpat, cq) =
	    (build_apat cpat, None, build_aval_pure cq) in
	Aval_at (loc, List.map build_case cases)
    | Cpred_be (loc, cx) ->
	build_aval_expr cx
    | Cpred_do1 (loc, _, _)
    | Cpred_do2 (loc, _, _, _)
    | Cpred_raise (loc, _) ->
	raise (Failure "Internal error: Should be pure.")
and build_aval_monad mm = function
    | Cpred_let (loc, None, cpat, crhs, ccont)
	    when not (Cst_utils.cpred_is_pure crhs) ->	(* Monadic Case *)
	(* Transform "let <var> be <rhs> in <cont>"
	 *        to "let tmp <var> be <cont> in <rhs> >>= tmp" *)
	if Cst_utils.count_formal_args cpat > 0 then
	    errf_at loc "Cannot run monadic action inside an abstraction. \
			 Maybe you meant to use 'let!'?";
	let vf = avar_for_line loc in
	let acont = build_aval_monad mm ccont in
	let acont = Aval_at (loc, [(build_apat cpat, None, acont)]) in
	let afref = Aval_ref (Apath ([], vf)) in
	let arhs = build_aval_monad (MM_bind afref) crhs in
	Aval_let (loc, Apat_uvar vf, acont, arhs)
    | Cpred_let (loc, cm_opt, cpat, cpred, ccont)	(* Pattern Case *)
	    when not (Cst_utils.is_formal cpat) ->
	let arhs = build_aval cm_opt cpred in
	let apat = build_apat cpat in
	let acont = build_aval_pure ccont in
	Aval_let (loc, apat, arhs, acont)
    | Cpred_let (loc, _, _, _, _) as clet ->		(* Recursive Case *)
	let rec loop bindings = function
	    | Cpred_let (loc, cm_opt, cpat, crhs, ccont)
		    when Cst_utils.is_formal cpat
		      && cm_opt <> None || Cst_utils.cpred_is_pure crhs ->
		let arhs = build_aval cm_opt crhs in
		let apat, arhs = wrap_abstractions cpat arhs in
		loop ((loc, apat, arhs) :: bindings) ccont
	    | ccont ->
		let acont = build_aval_monad mm ccont in
		let (last_loc, _, _) = List.hd bindings in
		let loc = Location.span [cpred_loc ccont; last_loc] in
		Aval_letrec (loc, List.rev bindings, acont) in
	loop [] clet
    | Cpred_if (loc, cond, cq, ccq) ->
	Aval_if (loc, build_aval_expr cond,
		 build_aval_monad mm cq, build_aval_monad mm ccq)
    | Cpred_at (loc, cases) ->
	let build_case (cpat, cq) =
	    (build_apat cpat, None, build_aval_monad mm cq) in
	begin match mm with
	| MM_quote _ -> Aval_at (loc, List.map build_case cases)
	| MM_bind _ ->
	    errf_at loc "Cannot run monadic action inside an abstraction. \
			 You may be missing a surrounding monad escape."
	end
    | Cpred_be (loc, cx) ->
	let ax = build_aval_expr cx in
	begin match mm with
	| MM_quote cm -> make_aval_return loc cm ax
	| MM_bind af -> Aval_apply (loc, af, ax)
	end
    | Cpred_do1 (loc, cm, cx) ->
	let ax = build_aval_expr cx in
	begin match mm with
	| MM_quote cm' ->
	    if cm <> cm' then errf_at loc "Mismatched monad indicator." else
	    ax
	| MM_bind af ->
	    make_aval_bind loc cm ax af
	end
    | Cpred_do2 (loc, cm, cx, cy) ->
	make_aval_chop loc cm (build_aval_expr cx) (build_aval_monad mm cy)
    | Cpred_raise (loc, _) -> raise (Failure "Unimplemented.")

and build_aval_expr = function
    | Ctrm_literal (loc, lit) ->
	Aval_literal (loc, lit)
    | Ctrm_ref (cidr, _) ->
	Aval_ref (Apath ([], cidr_to_avar cidr))
    | Ctrm_project _ as ctrm ->
	Aval_ref (build_apath ctrm)
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (op, _), cx), cy)
	    when cidr_is_2o_mapsto op ->
	build_aval_pure (Cpred_at (loc, [cx, Cpred_be (loc, cy)]))
    | Ctrm_apply (loc, cx, cy) ->
	Aval_apply (loc, build_aval_expr cx, build_aval_expr cy)
    | Ctrm_array (loc, cxs) ->
	Aval_array (loc, List.map build_aval_expr cxs)
    | Ctrm_rel (loc, cx, (_, cf, cy) :: rest) ->
	let build_aval_rel cf cx cy =
	    let loc = Location.span [ctrm_loc cx; ctrm_loc cy] in
	    let af = Aval_ref (Apath ([], cidr_to_avar cf)) in
	    let ax = build_aval_expr cx in
	    let ay = build_aval_expr cy in
	    Aval_apply (loc, Aval_apply (loc, af, ax), ay) in
	let rec build_conj aconj cx = function
	    | (_, cf, cy) :: rest ->
		let avar_2o_and = Avar (Location.dummy, idr_2o_and) in
		let and_op = Aval_ref (Apath ([], avar_2o_and)) in
		let arel = build_aval_rel cf cx cy in
		build_conj
		    (Aval_apply (loc, Aval_apply (loc, and_op, aconj), arel))
		    cy rest
	    | [] -> aconj in
	build_conj (build_aval_rel cf cx cy) cy rest
    | Ctrm_rel (_, _, []) -> invalid_arg "build_aval_expr"
    | Ctrm_what (loc, Some cm, cx) ->
	build_aval_monad (MM_quote cm) cx
    | Ctrm_what (loc, None, cx) ->
	build_aval_pure cx (* TODO *)
    | Ctrm_with (loc, _, _) | Ctrm_where (loc, _) ->
	errf_at loc "Module expressions are invalid in value terms."
    | Ctrm_quantify (loc, _, _, _)
    | Ctrm_label (loc, _, _) ->
	errf_at loc "UNIMPLEMENTED (label)"

module Algt_builder = struct
    type t = (atyp list * (loc * avar * atyp) list) Idr_map.t

    let empty = Idr_map.empty

    let add_type loc cp algtb =
	let ati = Atypinfo_abstract in
	let av, ats = build_atyp_con_args cp in
	let atcase = (loc, av, ats, ati) in
	(atcase, Idr_map.add (avar_idr av) (ats, []) algtb)

    let add_inj loc cf ct algtb =
	let af = build_avar cf in
	let at = build_atyp ct in
	let art = Ast_utils.result_type at in
	let ap, ats = Ast_utils.flatten_application art in
	let apts, injs =
	    try Idr_map.find (avar_idr (Ast_utils.apath_to_avar ap)) algtb
	    with Not_found ->
		let crt, _ = Cst_utils.flatten_arrow ct in
		errf_at loc "The type %s has not been defined in this scope."
			(Syn_print.ctrm_to_string crt) in
	Idr_map.add (avar_idr (apath_to_avar ap))
		    (ats, (loc, af, at) :: injs) algtb

    let find_injs av = Idr_map.find (avar_idr av)
end

let rec build_atcases is_sig atcases algtb = function
    | Cdef_type (loc, Ctrm_rel (_, p, [(_, op, ct)])) :: xs
	    when cidr_is_2o_eq op ->
	let ati = Atypinfo_alias (build_atyp ct) in
	let av, ats = build_atyp_con_args p in
	let atcase = (loc, av, ats, ati) in
	build_atcases is_sig (atcase :: atcases) algtb xs
    | Cdef_type (loc, p) :: xs ->
	let atcase, algtb' = Algt_builder.add_type loc p algtb in
	build_atcases is_sig (atcase :: atcases) algtb' xs
    | Cdef_inj (loc, Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (op, _), cf), ct))
	    :: xs when cidr_is_2o_colon op ->
	let algtb' = Algt_builder.add_inj loc cf ct algtb in
	build_atcases is_sig atcases algtb' xs
    | Cdef_inj (loc, _) :: _ ->
	errf_at loc "A type judgement expected after \"inj\"."
    | xs ->
	let finish_atcase = function
	    | loc, av, ats, Atypinfo_abstract ->
		let typinfo =
		    let ats, injs = Algt_builder.find_injs av algtb in
		    if injs = [] then
			if is_sig then Atypinfo_abstract else
			errf_at loc
				"The algebraic type %s needs at least one case."
				(avar_name av)
		    else Atypinfo_injs (List.rev injs)
		in (loc, av, ats, typinfo)
	    | atcase -> atcase in
	(List.rev_map finish_atcase atcases, xs)

let rec fold_on_comma f = function
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (op, _), x), y)
	    when cidr_is_2o_comma op ->
	fold_on_comma f x *> f y
    | x -> f x

let build_constraints loc eqns =
    fold_on_comma begin function
	| Ctrm_rel (loc, x, [(_, op, y)]) when cidr_is_2o_eq op ->
	    begin match x, y with
	    | Ctrm_apply (_, Ctrm_ref (op, _), x),
			  Ctrm_apply (_, Ctrm_ref (op', _), y)
		    when cidr_is_1o_asterisk op && cidr_is_1o_asterisk op' ->
		fun asig ->
		    Asig_with_struct (loc, asig, build_avar x, build_apath y)
	    | _ ->
		fun asig ->
		    Asig_with_type (loc, asig, build_atyp x, build_atyp y)
	    end
	| ctrm -> errf_at (ctrm_loc ctrm)
			  "Invalid constraint in signature expression"
    end eqns

let rec build_asig = function
    | Ctrm_ref (cidr, _) ->
	Asig_ref (Apath ([], cidr_to_avar cidr))
    | Ctrm_project _ as ctrm ->
	Asig_ref (build_apath ctrm)
    | Ctrm_with (loc, None, cdecs) ->
	Asig_decs (loc, build_adecs [] cdecs)
    | Ctrm_quantify (loc, q, cxvarsig, cysig)
	    when cidr_is_1q_functor q ->
	let cxvar, cxsig = Cst_utils.extract_term_typing cxvarsig in
	let axvar = build_avar cxvar in
	let axsig = build_asig cxsig in
	let aysig = build_asig cysig in
	Asig_product (loc, axvar, axsig, aysig)
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (op, _), csig), eqns)
		when cidr_is_2b_dotbracket op ->
	build_constraints loc eqns (build_asig csig)
    | ctrm -> errf_at (ctrm_loc ctrm) "Invalid signature expression."
and build_adecs adecs = function
    | Cdef_include (loc, csig) :: xs ->
	let adec = Adec_include (loc, build_asig csig) in
	build_adecs (adec :: adecs) xs
    | Cdef_open (loc, Abi_Fform, p) :: xs ->
	let adec = Adec_open (loc, build_apath p) in
	build_adecs (adec :: adecs) xs
    | Cdef_open (loc, Abi_C, x) :: xs ->
	errf_at loc "C ABI open is only valid in structures."
    | Cdef_in (loc, p, csig) :: xs ->
	let adec = Adec_in (loc, build_avar p, build_asig csig) in
	build_adecs (adec :: adecs) xs
    | Cdec_sig (loc, cidr) :: xs ->
	let adec = Adec_sig (loc, cidr_to_avar cidr, None) in
	build_adecs (adec :: adecs) xs
    | Cdef_sig (loc, cidr, csig) :: xs ->
	let asig = build_asig csig in
	let adec = Adec_sig (loc, cidr_to_avar cidr, Some asig) in
	build_adecs (adec :: adecs) xs
    | (Cdef_type _ :: _) as xs ->
	let atcases, xs' = build_atcases true [] Algt_builder.empty xs in
	let adec = Adec_types atcases in
	build_adecs (adec :: adecs) xs'
    | Cdef_val (loc, (`Local, _), _) :: xs ->
	build_adecs adecs xs
    | Cdef_val (loc, (expo, abi), cdec) :: xs ->
	let cv, ct = Cst_utils.extract_cidr_typing cdec in
	let av = cidr_to_avar cv in
	let at = build_atyp ct in
	let adec =
	    match abi with
	    | Abi_Fform -> Adec_val (loc, av, at)
	    | Abi_C -> Adec_cabi_val (loc, av, at) in
	build_adecs (adec :: adecs) xs
    | Cdef_let (loc, _, _, _) :: xs ->
	errf_at loc "Signatures cannot contain value definitions."
    | Cdef_inj (loc, _) :: xs ->
	errf_at loc "Injections must follow a type."
    | Cdef_lex (loc, _, _) :: xs -> build_adecs adecs xs
    | [] -> List.rev adecs


module Avcases_graph = struct
    type graph = (Idr_set.t * (loc * avar * atyp option * aval)) Idr_map.t
    type vertex = idr
    module Vertex_map = Idr_map

    let empty : graph = Idr_map.empty

    let fold_adjacent (g : graph) f v =
	try
	    let (vs, _) = Idr_map.find v g in
	    Idr_set.fold f vs
	with Not_found -> ident
end

module Avcases_algo = Graphalgo.ST_algo (Avcases_graph)

let fold_aval_deps f aval =
    let f' stratum path =
	match (stratum, path) with
	| `Value, Apath ([], Avar (_, idr)) -> f idr
	| _ -> ident in
    Ast_utils.fold_aval_paths f' aval

let break_rec_and_push_avcases avcases =
    (* Build dependency graph. *)
    let add_vertex ((_, Avar (_, v), _, aval) as avcase) (g, vs) =
	let deps = fold_aval_deps Idr_set.add aval Idr_set.empty in
	let g = Idr_map.add v (deps, avcase) g in
	(g, v :: vs) in
    let (g, vs) = List.fold add_vertex avcases (Avcases_graph.empty, []) in
    let vs = List.rev vs in

    (* Remove dependecies not included in g. *)
    let g = Idr_map.map
	(fun (vs, avcase) ->
	    (Idr_set.filter (fun v -> Idr_map.mem v g) vs, avcase)) g in

    (* Emit the components as individual Adef_letrec definitions. *)
    let vertex_deps v   = match Idr_map.find v g with (deps, _) -> deps in
    let vertex_avcase v = match Idr_map.find v g with (_, avcase) -> avcase in
    let push_component vs adefs =
	match vs with
	| [v] when not (Idr_set.mem v (vertex_deps v)) ->
	    let (loc, v, t_opt, x) = vertex_avcase v in
	    let pat =
		match t_opt with
		| None -> Apat_uvar v
		| Some t -> Apat_intype (loc, t, Apat_uvar v) in
	    Adef_let (loc, pat, x) :: adefs
	| _ -> Adef_letrec (List.map vertex_avcase vs) :: adefs in
    Avcases_algo.fold_strongly_connected g push_component vs

let wrap_amod_lambda ?loc_opt cxvarsig amod =
    let loc = Option.default (ctrm_loc cxvarsig) loc_opt in
    let cxvar, cxsig = Cst_utils.extract_term_typing cxvarsig in
    let axvar = build_avar cxvar in
    let axsig = build_asig cxsig in
    Amod_lambda (loc, axvar, axsig, amod)

let rec build_amod_of_pred = function
    | Cpred_at (loc, [cxvarsig, cymod]) ->
	wrap_amod_lambda cxvarsig (build_amod_of_pred cymod)
    | Cpred_be (loc, cmod) ->
	build_amod cmod
    | cpred -> errf_at (cpred_loc cpred) "Invalid module expression."
and build_amod = function
    | Ctrm_ref (cidr, _) ->
	Amod_ref (Apath ([], cidr_to_avar cidr))
    | Ctrm_project _ as ctrm ->
	Amod_ref (build_apath ctrm)
    | Ctrm_where (loc, cdefs) ->
	let adefs = build_adefs Idr_map.empty [] cdefs in
	Amod_defs (loc, adefs)
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (op, _), cx), cxsig)
	    when cidr_is_2o_colon op ->
	Amod_coercion (loc, build_amod cx, build_asig cxsig)
    | Ctrm_apply (loc, cf, cx) ->
	Amod_apply (loc, build_amod cf, build_amod cx)
    | ctrm -> errf_at (ctrm_loc ctrm) "Invalid module expression."

and build_toplevel_aval loc cm_opt cpred =
    begin match cm_opt with
    | Some cm ->
	build_aval_monad (MM_quote cm) cpred
    | None when Cst_utils.cpred_is_pure cpred ->
	build_aval_pure cpred
    | None ->
	let ax = build_aval_monad (MM_quote cmonad_io) cpred in
	let af = aval_ref_of_idr loc idr_run_action in
	Aval_apply (loc, af, ax)
    end
and build_adefs adecmap adefs = function
    | Cdef_include (loc, m) :: xs ->
	let adef = Adef_include (loc, build_amod m) in
	build_adefs adecmap (adef :: adefs) xs
    | Cdef_open (loc, Abi_Fform, p) :: xs ->
	let adef = Adef_open (loc, build_apath p) in
	build_adefs adecmap (adef :: adefs) xs
    | Cdef_open (loc, Abi_C, cinc) :: xs ->
	let adef =
	    match cinc with
	    | Ctrm_literal (_, Lit_string s) ->
		Adef_cabi_open (loc, (UString.to_utf8 s))
	    | _ -> errf_at loc "C ABI open takes a string argument." in
	build_adefs adecmap (adef :: adefs) xs
    | Cdef_in (loc, cpat, cmod) :: xs ->
	let cpat, cmod = Cst_utils.move_typing (cpat, cmod) in
	let amod = build_amod cmod in
	let cpat, amod =
	    Cst_utils.fold_formal_args wrap_amod_lambda (cpat, amod) in
	let adef = Adef_in (loc, build_avar cpat, amod) in
	build_adefs adecmap (adef :: adefs) xs
    | Cdec_sig (loc, cidr) :: xs ->
	errf_at loc "Missing signature definition."
    | Cdef_sig (loc, cidr, ctrm) :: xs ->
	let adef = Adef_sig (loc, cidr_to_avar cidr, build_asig ctrm) in
	build_adefs adecmap (adef :: adefs) xs
    | (Cdef_type _ :: _) as xs ->
	let atcases, xs' = build_atcases false [] Algt_builder.empty xs in
	let adef = Adef_types atcases in
	build_adefs adecmap (adef :: adefs) xs'
    | Cdef_val (loc, (expo, abi), cdec) :: xs ->
	let cv, ct = Cst_utils.extract_cidr_typing cdec in
	let at = build_atyp ct in
	let adefs =
	    match abi with
	    | Abi_Fform -> adefs
	    | Abi_C -> Adef_cabi_val (loc, cidr_to_avar cv, at) :: adefs in
	let adecmap = Idr_map.add (cidr_to_idr cv) (loc, at) adecmap in
	build_adefs adecmap adefs xs
    | Cdef_let (loc, cm_opt, cpat, cpred) :: xs
	    when not (Cst_utils.is_formal cpat) ->
	let apat = build_apat ~adecmap cpat in
	let aval = build_toplevel_aval loc cm_opt cpred in
	let adef = Adef_let (loc, apat, aval) in
	build_adefs adecmap (adef :: adefs) xs
    | (Cdef_let _ :: _) as xs ->
	let build_avcase = function
	    | Cdef_let (loc, cm_opt, cpat, cpred)
		    when Cst_utils.is_formal cpat ->
		let cvar, cpred = Cst_utils.move_applications (cpat, cpred) in
		let at_opt =
		    begin match cvar with
		    | Ctrm_ref (Cidr (_, idr), _) ->
			(try Some (snd (Idr_map.find idr adecmap))
			 with Not_found -> None)
		    | _ -> assert false (* unreachable *)
		    end in
		let avar = build_avar cvar in
		let aval = build_toplevel_aval loc cm_opt cpred in
		Some (loc, avar, at_opt, aval)
	    | _ -> None in
	let xs', avcases = List.map_while build_avcase xs in
	build_adefs adecmap (break_rec_and_push_avcases avcases adefs) xs'
    | Cdef_inj (loc, p) :: xs ->
	errf_at loc "Injections must follow a type definition."
    | Cdef_lex _ :: xs -> build_adefs adecmap adefs xs
    | [] ->
	let strip_used_avcase = function
	    | (_, v, Some _, _) -> Idr_map.remove (avar_idr v)
	    | _ -> ident in
	let strip_used_adef = function
	    | Adef_let (loc, pat, _) ->
		Ast_utils.fold_apat_typed_vars
		    (fun (t, v) -> Idr_map.remove (avar_idr v)) pat
	    | Adef_letrec avcases -> List.fold strip_used_avcase avcases
	    | Adef_cabi_val (loc, v, _) -> Idr_map.remove (avar_idr v)
	    | _ -> ident in
	let is_include = function Adef_include _ -> true | _ -> false in
	if not (List.exists is_include adefs) then begin
	    let adecmap = List.fold strip_used_adef adefs adecmap in
	    Idr_map.iter
		(fun idr (loc, _) ->
		    errf_at loc "Declaration lacks a subsequent definition.")
		adecmap
	end;
	List.rev adefs
