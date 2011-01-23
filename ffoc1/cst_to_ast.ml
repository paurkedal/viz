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
open Diag
open FfPervasives

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
    | _ -> raise (Failure "Unreachable.")

let rec build_atyp = function
    | Ctrm_ref (cidr, Ih_univ) ->
	Atyp_uvar (cidr_to_avar cidr)
    | Ctrm_ref (cidr, _) ->
	Atyp_ref (Apath ([], cidr_to_avar cidr))
    | Ctrm_project _ as ctrm ->
	Atyp_ref (build_apath ctrm)
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (op, _), ct), cu)
	    when cidr_is_2o_arrow op ->
	Atyp_arrow (loc, build_atyp ct, build_atyp cu)
    | Ctrm_apply (loc, ct, cu) ->
	Atyp_apply (loc, build_atyp ct, build_atyp cu)
    | ct -> errf_at (ctrm_loc ct) "Invalid type expression."

let build_atyp_con_args =
    let rec loop args = function
	| Ctrm_ref (Cidr (loc, idr), _) -> (Avar (loc, idr), args)
	| Ctrm_apply (loc, ct, arg) -> loop (build_atyp arg :: args) ct
	| ct -> errf_at (ctrm_loc ct) "Expecting a type constructor." in
    loop []

let rec build_apat ?(fpos = false) = function
    | Ctrm_ref (cidr, Ih_inj) ->
	Apat_ref (Apath ([], cidr_to_avar cidr))
    | Ctrm_ref (cidr, _) ->
	if fpos then Apat_ref (Apath ([], cidr_to_avar cidr))
	else         Apat_uvar (cidr_to_avar cidr)
    | Ctrm_project _ as ctrm ->
	Apat_ref (build_apath ctrm)
    | Ctrm_apply (loc, cx, cy) ->
	Apat_apply (loc, build_apat ~fpos:true cx, build_apat cy)
    | cx -> errf_at (ctrm_loc cx) "Invalid pattern."

let rec build_aval = function
    | Ctrm_literal (loc, lit) ->
	Aval_literal (loc, lit)
    | Ctrm_ref (cidr, _) ->
	Aval_ref (Apath ([], cidr_to_avar cidr))
    | Ctrm_project _ as ctrm ->
	Aval_ref (build_apath ctrm)
    | Ctrm_apply (loc, cx, cy) ->
	Aval_apply (loc, build_aval cx, build_aval cy)
    | Ctrm_rel (loc, cx, (_, cf, cy) :: rest) ->
	let build_aval_rel cf cx cy =
	    let loc = Location.span [ctrm_loc cx; ctrm_loc cy] in
	    let af = Aval_ref (Apath ([], cidr_to_avar cf)) in
	    let ax = build_aval cx in
	    let ay = build_aval cy in
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
    | Ctrm_rel (_, _, []) -> invalid_arg "build_aval"
    (*
    | Ctrm_lambda (loc, Ctrm_apply (_,
	    Ctrm_apply (_, Ctrm_ref (op, _), cxv), cxt), cy)
	    when cidr_is_2o_colon op ->
	Aval_lambda (loc, build_avar cxv, Some (build_atyp cxt), build_aval cy)
    *)
    | Ctrm_lambda (loc, cx, cy) ->
	Aval_at (loc, [build_apat cx, None, build_aval cy])
    | Ctrm_at (loc, cases) ->
	let build_case (pat, cq) = (build_apat pat, None, build_aval cq) in
	Aval_at (loc, List.map build_case cases)
    | Ctrm_let (loc, _, _, _) as clet ->
	let rec loop bindings = function
	    | Ctrm_let (loc, xpat, xbody, cy) ->
		let binding = (loc, build_apat xpat, build_aval xbody) in
		loop (binding :: bindings) cy
	    | cy ->
		Aval_let (loc, List.rev bindings, build_aval cy) in
	loop [] clet
    | Ctrm_if (loc, cond, cq, ccq) ->
	Aval_if (loc, build_aval cond, build_aval cq, build_aval ccq)
    | Ctrm_raise (loc, cexn) ->
	Aval_raise (loc, build_aval cexn)
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
			(ctrm_to_string crt) in
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
    | Cdef_open (loc, p) :: xs ->
	let adec = Adec_open (loc, build_apath p) in
	build_adecs (adec :: adecs) xs
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
    | Cdec_val (loc, cdec) :: xs ->
	let cv, ct = Cst_utils.extract_cidr_typing cdec in
	let adec = Adec_val (loc, cidr_to_avar cv, build_atyp ct) in
	build_adecs (adec :: adecs) xs
    | Cdef_val (loc, _, _) :: xs ->
	errf_at loc "Signatures cannot contain value definitions."
    | Cdef_inj (loc, _) :: xs ->
	errf_at loc "Injections must follow a type."
    | Cdef_lex (loc, _, _) :: xs -> build_adecs adecs xs
    | [] -> List.rev adecs


module Avcases_graph = struct
    type graph = (Idr_set.t * (loc * avar * aval)) Idr_map.t
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
    let f' = function
	| Apath ([], Avar (_, idr)) -> f idr
	| Apath _ -> ident in
    Ast_utils.fold_aval_paths f' aval

let break_rec_and_push_avcases avcases =
    (* Build dependency graph. *)
    let add_vertex ((_, Avar (_, v), aval) as avcase) (g, vs) =
	let deps = fold_aval_deps Idr_set.add aval Idr_set.empty in
	let g = Idr_map.add v (deps, avcase) g in
	(g, v :: vs) in
    let (g, vs) = List.fold add_vertex avcases (Avcases_graph.empty, []) in
    let vs = List.rev vs in

    (* Remove dependecies not included in g. *)
    let g = Idr_map.map
	(fun (vs, avcase) ->
	    (Idr_set.filter (fun v -> Idr_map.mem v g) vs, avcase)) g in

    (* Emit the components as individual Adef_vals definitions. *)
    let vertex_deps v   = match Idr_map.find v g with (deps, _) -> deps in
    let vertex_avcase v = match Idr_map.find v g with (_, avcase) -> avcase in
    let push_component vs adefs =
	match vs with
	| [v] when not (Idr_set.mem v (vertex_deps v)) ->
	    let (loc, v, x) = vertex_avcase v in
	    Adef_val (loc, v, x) :: adefs
	| _ -> Adef_vals (List.map vertex_avcase vs) :: adefs in
    Avcases_algo.fold_strongly_connected g push_component vs

let rec build_amod = function
    | Ctrm_ref (cidr, _) ->
	Amod_ref (Apath ([], cidr_to_avar cidr))
    | Ctrm_project _ as ctrm ->
	Amod_ref (build_apath ctrm)
    | Ctrm_where (loc, cdefs) ->
	Amod_defs (loc, build_adefs [] cdefs)
    | Ctrm_apply (loc, Ctrm_apply (_, Ctrm_ref (op, _), cx), cxsig)
	    when cidr_is_2o_colon op ->
	Amod_coercion (loc, build_amod cx, build_asig cxsig)
    | Ctrm_apply (loc, cf, cx) ->
	Amod_apply (loc, build_amod cf, build_amod cx)
    | Ctrm_lambda (loc, cxvarsig, cymod) ->
	let cxvar, cxsig = Cst_utils.extract_term_typing cxvarsig in
	let axvar = build_avar cxvar in
	let axsig = build_asig cxsig in
	let aymod = build_amod cymod in
	Amod_lambda (loc, axvar, axsig, aymod)
    | ctrm -> errf_at (ctrm_loc ctrm) "Invalid module expression."

and build_adefs adefs = function
    | Cdef_include (loc, m) :: xs ->
	let adef = Adef_include (loc, build_amod m) in
	build_adefs (adef :: adefs) xs
    | Cdef_open (loc, p) :: xs ->
	let adef = Adef_open (loc, build_apath p) in
	build_adefs (adef :: adefs) xs
    | Cdef_in (loc, p, m) :: xs ->
	let p, m = Cst_utils.move_typing (p, m) in
	let p, m = Cst_utils.move_applications (p, m) in
	let adef = Adef_in (loc, build_avar p, build_amod m) in
	build_adefs (adef :: adefs) xs
    | Cdec_sig (loc, cidr) :: xs ->
	errf_at loc "Missing signature definition."
    | Cdef_sig (loc, cidr, ctrm) :: xs ->
	let adef = Adef_sig (loc, cidr_to_avar cidr, build_asig ctrm) in
	build_adefs (adef :: adefs) xs
    | (Cdef_type _ :: _) as xs ->
	let atcases, xs' = build_atcases false [] Algt_builder.empty xs in
	let adef = Adef_types atcases in
	build_adefs (adef :: adefs) xs'
    | Cdec_val (loc, p) :: xs -> errf_at loc "UNIMPLEMENTED (val)"
    | (Cdef_val _ :: _) as xs ->
	let build_avcase = function
	    | Cdef_val (loc, cpat, ctrm) ->
		let cvar, ctrm = Cst_utils.move_typing (cpat, ctrm) in
		let cvar, ctrm = Cst_utils.move_applications (cvar, ctrm) in
		let avar = build_avar cvar in
		let aval = build_aval ctrm in
		Some (loc, avar, aval)
	    | _ -> None in
	let xs', avcases = List.map_while build_avcase xs in
	build_adefs (break_rec_and_push_avcases avcases adefs) xs'
    | Cdef_inj (loc, p) :: xs ->
	errf_at loc "Injections must follow a type definition."
    | Cdef_lex _ :: xs ->
	build_adefs adefs xs
    | [] -> List.rev adefs
