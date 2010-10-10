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

open Camlp4.PreCast
open Input
open Unicode
open FfPervasives
open Printf

exception Error of Location.t * string

type approx_type =
    | At_sig
    | At_struct of approx_type Idr_map.t
    | At_type
    | At_val
    | At_inj of int

let ctor_name s = String.capitalize s

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

    let rec add_ctyp idr ctyp builder =
	match builder.accu with
	| Accu_none ->
	    { builder with accu = Accu_ctyp [ctyp] }
	| Accu_ctyp ctyps ->
	    { builder with accu = Accu_ctyp (ctyp :: ctyps) }
	| _ ->
	    add_ctyp idr ctyp (flush builder)

    let rec add_binding binding builder =
	match builder.accu with
	| Accu_none ->
	    { builder with accu = Accu_binding [binding] }
	| Accu_binding bindings ->
	    { builder with accu = Accu_binding (binding :: bindings) }
	| _ ->
	    add_binding binding (flush builder)

    let add_def _loc idr binding at builder =
	{ add_binding binding builder with
	    env = Idr_map.add idr at builder.env }

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

let extract_term_typing = function
    | Trm_apply (_, Trm_apply (_, Trm_ref (_, colon, _), x), y)
	    when colon = i_2o_colon ->
	(x, y)
    | trm -> raise (Error (trm_location trm, "Type judgement expected."))
let extract_idr_typing expr =
    match extract_term_typing expr with
    | Trm_ref (_, idr, _), y -> (idr, y)
    | x, y -> raise (Error (trm_location x, "Identifier expected."))

let flatten_tycon_apply typ =
    let rec loop tps = function
	| Trm_apply (_, typ, tp) -> loop (tp :: tps) typ
	| Trm_ref (_, c, _) -> (c, List.rev tps)
	| trm ->
	    raise (Error (trm_location trm, "Not a type constructor.")) in
    loop [] typ

let rec gen_ctyp = function
    | Trm_ref (loc, Idr c, Ih_none) ->
	let _loc = convert_loc loc in
	if String.get c 0 = '\'' then
	    (Idr c, <:ctyp< '$lid: String.sub c 1 (String.length c - 1)$ >>)
	else
	    (Idr c, <:ctyp< $lid:c$ >>)
    | Trm_apply (loc, x, y) ->
	let _loc = convert_loc loc in
	let (idr, x') = gen_ctyp x in
	let (_,   y') = gen_ctyp y in
	(idr, <:ctyp< $x'$ $y'$ >>)
    | Trm_rel (loc, x, [(_, r, y)]) when r = i_2o_eq ->
	let _loc = convert_loc loc in
	let (tycon, typrms) = flatten_tycon_apply x in
	let typrms' = List.map (snd *< gen_ctyp) typrms in
	let (idr, x') = gen_ctyp x in
	let (_,   y') = gen_ctyp y in
	(idr, Ast.TyDcl (_loc, idr_to_string tycon, typrms', y', []))
    | trm ->
	raise (Error (trm_location trm, "Ivalid type expression."))

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
let gen_name _loc name =
    if String.length name > 2 && ('1' <= name.[0] && name.[0] <= '3')
	    && name.[1] = '\''
    then gen_opname _loc name
    else <:expr< $lid:name$ >>

let rec gen_pattern ?(isf = false) env = function
    | Trm_ref (loc, Idr idr, hint) when  hint = Ih_inj
				     || (hint = Ih_none && isf) ->
	let _loc = convert_loc loc in
	<:patt< $uid:ctor_name idr$ >>
    | Trm_ref (loc, Idr idr, hint) when hint <> Ih_inj ->
	let _loc = convert_loc loc in
	<:patt< $lid:idr$ >>
    | Trm_literal (loc, lit) ->
	let _loc = convert_loc loc in
	gen_literal_patt _loc lit
    | Trm_apply (loc, f, x) ->
	let _loc = convert_loc loc in
	let f' = gen_pattern ~isf:true  env f in
	let x' = gen_pattern ~isf:false env x in
	<:patt< $f'$ $x'$ >>
    | trm -> raise (Error (trm_location trm, "Unimplemented pattern."))

let rec gen_sig_expr env = function
    | Trm_ref (loc, Idr name, Ih_none) ->
	let _loc = convert_loc loc in
	<:module_type< $uid: String.capitalize name$ >>
    | trm ->
	raise (Error (trm_location trm, "Invalid signature expression."))
let rec gen_struct_expr env = function
    | Trm_ref (loc, Idr name, Ih_none) ->
	let _loc = convert_loc loc in
	<:module_expr< $uid: String.capitalize name$ >>
    | Trm_apply (loc, f, m) ->
	let _loc = convert_loc loc in
	<:module_expr< $gen_struct_expr env f$ $gen_struct_expr env m$ >>
    | Trm_lambda (loc, pat, body) ->
	let _loc = convert_loc loc in
	begin match pat with
	| Trm_apply (locp, Trm_apply (locx, Trm_ref (_, colon, _),
					    Trm_ref (_, Idr m, _)),
			   sgt)
		when colon = i_2o_colon ->
	    let sgt' = gen_sig_expr env sgt in
	    let body' = gen_struct_expr env body in
	    <:module_expr< functor ($uid:m$ : $sgt'$) -> $body'$ >>
	| _ ->
	    raise (Error (trm_location pat, "Invalid functor parameter."))
	end
    | trm ->
	raise (Error (trm_location trm, "Invalid structure expression."))
and gen_expr env = function
    | Trm_ref (loc, Idr name, Ih_none) ->
	let _loc = convert_loc loc in
	gen_name _loc name
    | Trm_literal (loc, lit) ->
	let _loc = convert_loc loc in
	gen_literal_expr _loc lit
    | Trm_project (loc, Idr field, m) ->
	let _loc = convert_loc loc in
	let m' = gen_struct_expr env m in
	<:expr< let module M_ = $mexp: m'$ in M_.$lid:field$ >>
    | Trm_lambda (loc, pat, body) ->
	let _loc = convert_loc loc in
	let pat' = gen_pattern env pat in
	let body' = gen_expr env body in
	<:expr< fun [ $pat'$ -> $body'$ ] >>
    | Trm_let (loc, pat, dfi, scp) ->
	let _loc = convert_loc loc in
	let pat' = gen_pattern env pat in
	let dfi' = gen_expr env dfi in
	let scp' = gen_expr env scp in
	<:expr< let $pat'$ = $dfi'$ in $scp'$ >>
    | Trm_rel (loc, x, rels) ->
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
    | Trm_apply (loc, f, x) ->
	let _loc = convert_loc loc in
	let f' = gen_expr env f in
	let x' = gen_expr env x in
	<:expr< $f'$ $x'$ >>
    | Trm_if (loc, cond, cq, ccq) ->
	let _loc = convert_loc loc in
	let cond' = gen_expr env cond in
	let cq' = gen_expr env cq in
	let ccq' = gen_expr env ccq in
	<:expr< if $cond'$ then $cq'$ else $ccq'$ >>
    | Trm_at (loc, cases) ->
	let _loc = convert_loc loc in
	let cases' = gen_cases env _loc cases in
	<:expr< fun [ $list:cases'$ ] >>
    | trm -> raise (Error (trm_location trm, "Unimplemented expression."))
and gen_cases env _loc = List.map
    (fun (pat, cq) ->
	let pat' = gen_pattern env pat in
	let cq' = gen_expr env cq in
	<:match_case< $pat'$ -> $cq'$ >>)
and gen_apply2_idr env _loc (Idr op) x0 x1 =
    let f = gen_name _loc op in
    let x0' = gen_expr env x0 in
    let x1' = gen_expr env x1 in
    <:expr< $f$ $x0'$ $x1'$ >>

let collect_inj def inj_map =
    match def with
    | Dec_inj (loc, trm) ->
	let _loc = convert_loc loc in
	let (Idr injname, typ) = extract_idr_typing trm in
	let rec flatten_arrow pts = function
	    | Trm_apply (_, Trm_apply (_, Trm_ref (_, c, _), pt), rt)
		    when c = i_2o_arrow ->
		let (_, pt') = gen_ctyp pt in
		flatten_arrow (pt' :: pts) rt
	    | rt -> (rt, List.rev pts) in
	let (rt, pts) = flatten_arrow [] typ in
	let (rc, tps) = flatten_tycon_apply rt in
	let injs = try Idr_map.find rc inj_map with Not_found -> [] in
	let inj = <:ctyp< $uid:ctor_name injname$ of $list:pts$ >> in
	Idr_map.add rc (inj :: injs) inj_map
    | _ -> inj_map

let gen_val_def = function
    | Dec_lex _ -> ident
    | Dec_type (loc, typ) -> fun builder ->
	let _loc = convert_loc loc in
	let (idr, ctyp) = gen_ctyp typ in
	let ctyp' =
	    try
		let injs = Idr_map.find idr builder.Struct_builder.inj_map in
		let (Idr tycon, typrms) = flatten_tycon_apply typ in
		let typrms' = List.map (snd *< gen_ctyp) typrms in
		Ast.TyDcl (_loc, tycon, typrms', <:ctyp< [ $list:injs$ ] >>, [])
	    with Not_found -> ctyp in
	Struct_builder.add_ctyp idr ctyp' builder
    | Def_val (loc, pat, body) -> fun builder ->
	let _loc = convert_loc loc in
	let body' = gen_expr builder.Struct_builder.env body in
	let rec decons_formal body' = function
	    | Trm_apply (loc, f, x) ->
		let _loc = convert_loc loc in
		let x' = gen_pattern builder.Struct_builder.env x in
		decons_formal <:expr< fun $x'$ -> $body'$ >> f
	    | Trm_ref (loc, f, Ih_none) ->
		(f, body')
	    | _ -> raise (Error (loc, "Invalid pattern in formals.")) in
	let (f, body'') = decons_formal body' pat in
	let binding' = <:binding< $lid:idr_to_string f$ = $body''$ >> in
	Struct_builder.add_def _loc f binding' At_val builder
    | Dec_inj (loc, typing) -> fun builder ->
	let _loc = convert_loc loc in
	let idr, typ = extract_idr_typing typing in
	let v = idr_to_string idr in
	let c = ctor_name v in
	let r = application_depth 1 i_2o_arrow typ in
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
	Struct_builder.add_def _loc idr binding' (At_inj r) builder
    | Def_type(loc, _, _) -> fun builder ->
	raise (Error (loc, "Not implemented."))

let gen_toplevel = function
    | Trm_where (loc, defs) ->
	let _loc = convert_loc loc in
	let inj_map = List.fold collect_inj defs Idr_map.empty in
	let builder = Struct_builder.create inj_map in
	let builder' = List.fold gen_val_def defs builder in
	Struct_builder.get_str_item _loc builder'
    | trm ->
	raise (Error (trm_location trm, "Expecting a structure."))
