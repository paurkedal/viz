(* Copyright 2010  Petter Urkedal
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
open Diag
open Cst_types
open Cst
open Cst_utils
open Unicode
open FfPervasives
open Printf

type approx_type =
    | At_sig
    | At_struct of approx_type Idr_map.t
    | At_type
    | At_val
    | At_inj of int

module Struct_builder = struct
    type accu =
	| Accu_none
	| Accu_ctyp of Ast.ctyp list
	| Accu_binding of Ast.binding list

    type t = {
	inj_map : Ast.ctyp list Idr_map.t;
	env : approx_type Idr_map.t;
	accu : accu;
	items : Ast.str_item list;
    }

    let create inj_map =
	{
	    inj_map = inj_map;
	    env = Idr_map.empty;
	    accu = Accu_none;
	    items = [];
	}

    let flush builder =
	let item =
	    let _loc = Loc.ghost in
	    match builder.accu with
	    | Accu_ctyp ctyps ->
		<:str_item< type $list:List.rev ctyps$ >>
	    | Accu_binding bindings ->
		<:str_item< value rec $list:List.rev bindings$ >>
	    | _ ->
		raise (Failure "Nothing to flush.") in
	{ builder with
	    items = item :: builder.items;
	    accu = Accu_none;
	}

    let add_str_item item builder =
	let builder' = if builder.accu <> Accu_none then flush builder
		       else builder in
	{ builder' with items = item :: builder.items; }

    let rec add_ctyp cidr ctyp builder =
	match builder.accu with
	| Accu_none ->
	    { builder with accu = Accu_ctyp [ctyp] }
	| Accu_ctyp ctyps ->
	    { builder with accu = Accu_ctyp (ctyp :: ctyps) }
	| _ ->
	    add_ctyp cidr ctyp (flush builder)

    let rec add_binding binding builder =
	match builder.accu with
	| Accu_none ->
	    { builder with accu = Accu_binding [binding] }
	| Accu_binding bindings ->
	    { builder with accu = Accu_binding (binding :: bindings) }
	| _ ->
	    add_binding binding (flush builder)

    let add_def _loc (Cidr (_, idr)) binding at builder =
	{ add_binding binding builder with
	    env = Idr_map.add idr at builder.env }

    let find_injs (Cidr (_, idr)) builder = Idr_map.find idr builder.inj_map

    let get_str_item _loc builder =
	let builder' =
	    if builder.accu <> Accu_none then flush builder else builder in
	<:str_item< $list:List.rev builder'.items$ >>
end

let convert_loc loc =
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

let fresh_var =
    let next_index = ref 0 in
    fun () ->
	let i = !next_index in
	next_index := i + 1;
	sprintf "_x%d" i

let str_to_lid s = String.uncapitalize s
let str_to_uid s = String.capitalize s
let idr_to_uc (Idr s) = str_to_uid s
let idr_to_lc (Idr s) = str_to_lid s
let cidr_to_uc (Cidr (_, Idr s)) = str_to_uid s
let cidr_to_lc (Cidr (_, Idr s)) = str_to_lid s

let gen_name is_uid (Cidr (loc, Idr name)) =
    let _loc = convert_loc loc in
    if is_uid then
	<:ident< $uid: String.capitalize name$ >>
    else
	<:ident< $lid: name$ >>
let rec gen_ident is_uid = function
    | Ctrm_project (loc, field, m) ->
	let _loc = convert_loc loc in
	<:ident< $gen_ident true m$ . $gen_name is_uid field$ >>
    | Ctrm_ref (cidr, hint) ->
	gen_name (hint = Ih_inj) cidr
    | ctrm -> errf_at (trm_location ctrm) "Not a path."

let rec gen_ctyp = function
    | Ctrm_ref (cidr, Ih_none) ->
	let _loc = convert_loc (cidr_location cidr) in
	let s = cidr_to_string cidr in
	if String.get s 0 = '\'' then
	    let s' = String.sub s 1 (String.length s - 1) in
	    (cidr, <:ctyp< '$lid: str_to_lid s'$ >>)
	else
	    (cidr, <:ctyp< $lid: str_to_lid s$ >>)
    | Ctrm_apply (loc, x, y) ->
	let _loc = convert_loc loc in
	let (name, x') = gen_ctyp x in
	let (_,   y') = gen_ctyp y in
	(name, <:ctyp< $x'$ $y'$ >>)
    | Ctrm_rel (loc, x, [(_, r, y)]) when cidr_is_2o_eq r ->
	let _loc = convert_loc loc in
	let (tycon, typrms) = flatten_tycon_application x in
	let typrms' = List.map (snd *< gen_ctyp) typrms in
	let (name, x') = gen_ctyp x in
	let (_,   y') = gen_ctyp y in
	(name, Ast.TyDcl (_loc, cidr_to_string tycon, typrms', y', []))
    | ctrm ->
	errf_at (trm_location ctrm) "Ivalid type expression."

let gen_literal_patt _loc = function
    | Lit_unit     -> <:patt< () >>
    | Lit_bool x   -> if x then <:patt< True >> else <:patt< False >>
    | Lit_int x    -> let s = string_of_int x in <:patt< $int:s$ >>
    | Lit_float x  -> let s = string_of_float x in <:patt< $flo:s$ >>
    | Lit_string x -> let s = UString.to_utf8 x in <:patt< $str:s$ >>

let gen_literal_expr _loc = function
    | Lit_unit     -> <:expr< () >>
    | Lit_bool x   -> if x then <:expr< True >> else <:expr< False >>
    | Lit_int x    -> let s = string_of_int x in <:expr< $int:s$ >>
    | Lit_float x  -> let s = string_of_float x in <:expr< $flo:s$ >>
    | Lit_string x -> let s = UString.to_utf8 x in <:expr< $str:s$ >>

let gen_opname _loc = function
    | "1'¬" -> <:expr< not >>
    | "2'∧" -> <:expr< (&&) >>
    | "2'∨" -> <:expr< (||) >>
    | "2'=" -> <:expr< (=) >>
    | "2'≠" -> <:expr< (<>) >>
    | "2'<" -> <:expr< (<) >>
    | "2'>" -> <:expr< (>) >>
    | "2'≤" -> <:expr< (<=) >>
    | "2'≥" -> <:expr< (>=) >>
    | "2'+" -> <:expr< (+) >>
    | "1'-" -> <:expr< (~-) >>
    | "2'-" -> <:expr< (-) >>
    | "2'*" -> <:expr< ( * ) >>
    | "2'/" -> <:expr< (/) >>
    | "2'mod" -> <:expr< (mod) >>
    | s ->
	let buf = Buffer.create 0 in
	Buffer.add_char buf '_';
	Buffer.add_string buf (String.sub s 0 2);
	Buffer.add_char buf '_';
	for i = 2 to String.length s - 1 do
	    bprintf buf "%02x" (Char.code s.[i])
	done;
	<:expr< $lid:Buffer.contents buf$ >>
let gen_value_name (Cidr (loc, idr)) =
    let _loc = convert_loc loc in
    let name = idr_to_string idr in
    if String.length name > 2 && ('1' <= name.[0] && name.[0] <= '3')
	    && name.[1] = '\''
    then gen_opname _loc name
    else <:expr< $lid: idr_to_lc idr$ >>

let rec gen_pattern ?(isf = false) env = function
    | Ctrm_ref (Cidr (loc, idr), hint)
	    when  hint = Ih_inj || (hint = Ih_none && isf) ->
	let _loc = convert_loc loc in
	<:patt< $uid: idr_to_uc idr$ >>
    | Ctrm_ref (Cidr (loc, idr), hint) when hint <> Ih_inj ->
	let _loc = convert_loc loc in
	<:patt< $lid: idr_to_lc idr$ >>
    | Ctrm_literal (loc, lit) ->
	let _loc = convert_loc loc in
	gen_literal_patt _loc lit
    | Ctrm_apply (loc, f, x) ->
	let _loc = convert_loc loc in
	let f' = gen_pattern ~isf:true  env f in
	let x' = gen_pattern ~isf:false env x in
	<:patt< $f'$ $x'$ >>
    | ctrm -> errf_at (trm_location ctrm) "Unimplemented pattern."

let rec gen_sig_expr env = function
    | Ctrm_ref (Cidr (loc, idr), Ih_none) ->
	let _loc = convert_loc loc in
	<:module_type< $uid: idr_to_uc idr$ >>
    | ctrm ->
	errf_at (trm_location ctrm) "Invalid signature expression %s."
		(trm_to_string ctrm)
let rec gen_struct_expr env = function
    | Ctrm_ref (Cidr (loc, idr), Ih_none) ->
	let _loc = convert_loc loc in
	<:module_expr< $uid: idr_to_uc idr$ >>
    | Ctrm_apply (loc, Ctrm_apply (loc1, Ctrm_ref (op, _), m), s)
	    when cidr_is_2o_colon op ->
	let _loc = convert_loc loc in
	<:module_expr< ($gen_struct_expr env m$ : $gen_sig_expr env s$) >>
    | Ctrm_apply (loc, f, m) ->
	let _loc = convert_loc loc in
	<:module_expr< $gen_struct_expr env f$ $gen_struct_expr env m$ >>
    | Ctrm_lambda (loc, pat, body) ->
	let _loc = convert_loc loc in
	begin match pat with
	| Ctrm_apply (locp,
		Ctrm_apply (locx, Ctrm_ref (colon, _), Ctrm_ref (m, _)), sgt)
		when cidr_is_2o_colon colon ->
	    let sgt' = gen_sig_expr env sgt in
	    let body' = gen_struct_expr env body in
	    <:module_expr< functor ($uid: cidr_to_uc m$ : $sgt'$) -> $body'$ >>
	| _ ->
	    errf_at (trm_location pat) "Invalid functor parameter."
	end
    | ctrm ->
	errf_at (trm_location ctrm) "Invalid structure expression %s."
		(trm_to_string ctrm)
and gen_expr env = function
    | Ctrm_ref (cidr, Ih_none) ->
	gen_value_name cidr
    | Ctrm_literal (loc, lit) ->
	let _loc = convert_loc loc in
	gen_literal_expr _loc lit
    | Ctrm_project (loc, Cidr (_, Idr field), m) ->
	let _loc = convert_loc loc in
	let m' = gen_struct_expr env m in
	<:expr< let module M_ = $mexp: m'$ in M_.$lid:field$ >>
    | Ctrm_lambda (loc, pat, body) ->
	let _loc = convert_loc loc in
	let pat' = gen_pattern env pat in
	let body' = gen_expr env body in
	<:expr< fun [ $pat'$ -> $body'$ ] >>
    | Ctrm_let (loc, pat, dfi, scp) ->
	let _loc = convert_loc loc in
	let pat' = gen_pattern env pat in
	let dfi' = gen_expr env dfi in
	let scp' = gen_expr env scp in
	<:expr< let $pat'$ = $dfi'$ in $scp'$ >>
    | Ctrm_rel (loc, x, rels) ->
	let _loc = convert_loc loc in
	begin match
	    List.fold begin fun (_, op, y) (accu, x) ->
		let rel = gen_apply2_idr env _loc op x y in
		match accu with
		| Some accu' -> (Some <:expr< $accu'$ && $rel$ >>, y)
		| None -> (Some rel, y)
	    end rels (None, x)
	with
	| (Some accu, _) -> accu
	| _ -> raise (Failure "Malformed relation expression.")
	end
    | Ctrm_apply (loc, f, x) ->
	let _loc = convert_loc loc in
	let f' = gen_expr env f in
	let x' = gen_expr env x in
	<:expr< $f'$ $x'$ >>
    | Ctrm_if (loc, cond, cq, ccq) ->
	let _loc = convert_loc loc in
	let cond' = gen_expr env cond in
	let cq' = gen_expr env cq in
	let ccq' = gen_expr env ccq in
	<:expr< if $cond'$ then $cq'$ else $ccq'$ >>
    | Ctrm_at (loc, cases) ->
	let _loc = convert_loc loc in
	let cases' = gen_cases env _loc cases in
	<:expr< fun [ $list:cases'$ ] >>
    | ctrm -> errf_at (trm_location ctrm) "Unimplemented expression."
and gen_cases env _loc = List.map
    (fun (pat, cq) ->
	let pat' = gen_pattern env pat in
	let cq' = gen_expr env cq in
	<:match_case< $pat'$ -> $cq'$ >>)
and gen_apply2_idr env _loc op x0 x1 =
    let f = gen_value_name op in
    let x0' = gen_expr env x0 in
    let x1' = gen_expr env x1 in
    <:expr< $f$ $x0'$ $x1'$ >>

let collect_inj cdef inj_map =
    match cdef with
    | Cdef_inj (loc, ctrm) ->
	let _loc = convert_loc loc in
	let (name, typ) = extract_cidr_typing ctrm in
	let rec flatten_arrow pts = function
	    | Ctrm_apply (_, Ctrm_apply (_, Ctrm_ref (op, _), pt), rt)
		    when cidr_is_2o_arrow op ->
		let (_, pt') = gen_ctyp pt in
		flatten_arrow (pt' :: pts) rt
	    | rt -> (rt, List.rev pts) in
	let (rt, pts) = flatten_arrow [] typ in
	let (Cidr (_, rc), tps) = flatten_tycon_application rt in
	let injs = try Idr_map.find rc inj_map with Not_found -> [] in
	let inj = <:ctyp< $uid: cidr_to_uc name$ of $list:pts$ >> in
	Idr_map.add rc (inj :: injs) inj_map
    | _ -> inj_map

let gen_val_def = function
    | Cdef_open (loc, path) ->
	let _loc = convert_loc loc in
	Struct_builder.add_str_item <:str_item< open $gen_ident true path$ >>
    | Cdef_include (loc, path) ->
	let _loc = convert_loc loc in
	Struct_builder.add_str_item
		<:str_item< include $id: gen_ident true path$ >>
    | Cdef_in (loc, pat, body) ->
	let _loc = convert_loc loc in
	let pat, body = move_typing (pat, body) in
	let pat, body = move_applications (pat, body) in
	begin match pat with
	| Ctrm_ref (name, _) -> fun builder ->
	    let body' = gen_struct_expr builder.Struct_builder.env body in
	    Struct_builder.add_str_item
		<:str_item< module $uid: cidr_to_uc name$ = $body'$ >>
		builder
	| _ ->
	    errf_at loc "Invalid head %s of module pattern." (trm_to_string pat)
	end
    | Cdef_lex _ -> ident
    | Cdef_type (loc, typ) -> fun builder ->
	let _loc = convert_loc loc in
	let (name, ctyp) = gen_ctyp typ in
	let ctyp' =
	    try
		let injs = Struct_builder.find_injs name builder in
		let (Cidr (_, Idr c), typrms) = flatten_tycon_application typ in
		let typrms' = List.map (snd *< gen_ctyp) typrms in
		Ast.TyDcl (_loc, c, typrms', <:ctyp< [ $list:injs$ ] >>, [])
	    with Not_found -> ctyp in
	Struct_builder.add_ctyp name ctyp' builder
    | Cdef_val (loc, pat, body) -> fun builder ->
	let _loc = convert_loc loc in
	let body' = gen_expr builder.Struct_builder.env body in
	let rec decons_formal body' = function
	    | Ctrm_apply (loc, f, x) ->
		let _loc = convert_loc loc in
		let x' = gen_pattern builder.Struct_builder.env x in
		decons_formal <:expr< fun $x'$ -> $body'$ >> f
	    | Ctrm_ref (f, Ih_none) ->
		(f, body')
	    | _ -> errf_at (loc) "Invalid pattern in formals." in
	let (f, body'') = decons_formal body' pat in
	let binding' = <:binding< $lid: cidr_to_string f$ = $body''$ >> in
	Struct_builder.add_def _loc f binding' At_val builder
    | Cdef_inj (loc, typing) -> fun builder ->
	let _loc = convert_loc loc in
	let name, typ = extract_cidr_typing typing in
	let v = cidr_to_lc name in
	let c = cidr_to_uc name in
	let r = application_depth 1 idr_2o_arrow typ in
	let binding' =
	    if r = 0 then
		<:binding< $lid:v$ = $uid:c$ >>
	    else begin
		let parms = repeat r
		    (fun vs -> let v = fresh_var () in v :: vs)
		    [] in
		let e0 =
		    List.fold (fun v e -> <:expr< $e$ $lid:v$ >>)
			      parms <:expr< $uid:c$ >> in
		let e1 =
		    List.fold (fun p e -> <:expr< fun $lid:p$ -> $e$ >>)
			      parms e0 in
		<:binding< $lid:v$ = $e1$ >>
	    end in
	Struct_builder.add_def _loc name binding' (At_inj r) builder

let gen_toplevel = function
    | Ctrm_where (loc, defs) ->
	let _loc = convert_loc loc in
	let inj_map = List.fold collect_inj defs Idr_map.empty in
	let builder = Struct_builder.create inj_map in
	let builder' = List.fold gen_val_def defs builder in
	Struct_builder.get_str_item _loc builder'
    | ctrm ->
	errf_at (trm_location ctrm) "Expecting a structure."
