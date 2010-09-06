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

let ctor_name s = String.capitalize s ^ "_"

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
		<:str_item< value $list:List.rev bindings$ >>
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

let rec gen_ctyp = function
    | Trm_ref (loc, Idr c) ->
	let _loc = convert_loc loc in
	(Idr c, <:ctyp< $lid:c$ >>)
    | Trm_apply (loc, x, Trm_ref (_, Idr y)) ->
	let _loc = convert_loc loc in
	let (idr, x') = gen_ctyp x in
	(idr, <:ctyp< $x'$ '$y$ >>)
    | Trm_rel (loc, r, x, y) when r = i_2o_eq ->
	let _loc = convert_loc loc in
	let (idr, x') = gen_ctyp x in
	let (_,   y') = gen_ctyp y in
	(idr, <:ctyp< $x'$ == $y'$ >>)
    | trm ->
	raise (Error (trm_location trm, "Ivalid type expression."))

let gen_literal_patt _loc = function
    | Lit_unit     -> <:patt< () >>
    | Lit_int x    -> let s = string_of_int x in <:patt< $int:s$ >>
    | Lit_float x  -> let s = string_of_float x in <:patt< $flo:s$ >>
    | Lit_string x -> let s = UString.to_utf8 x in <:patt< $str:s$ >>

let gen_literal_expr _loc = function
    | Lit_unit     -> <:expr< () >>
    | Lit_int x    -> let s = string_of_int x in <:expr< $int:s$ >>
    | Lit_float x  -> let s = string_of_float x in <:expr< $flo:s$ >>
    | Lit_string x -> let s = UString.to_utf8 x in <:expr< $str:s$ >>

let rec gen_pattern env = function
    | Trm_ref (loc, Idr idr) ->
	let _loc = convert_loc loc in
	<:patt< $lid:idr$ >>
    | Trm_literal (loc, lit) ->
	let _loc = convert_loc loc in
	gen_literal_patt _loc lit
    | Trm_apply (loc, f, x) ->
	let _loc = convert_loc loc in
	let f' = gen_pattern env f in
	let x' = gen_pattern env x in
	<:patt< $f'$ $x'$ >>
    | trm -> raise (Error (trm_location trm, "Unimplemented pattern."))

let rec gen_expr env = function
    | Trm_ref (loc, Idr name) ->
	let _loc = convert_loc loc in
	<:expr< $lid:name$ >>
    | Trm_literal (loc, lit) ->
	let _loc = convert_loc loc in
	gen_literal_expr _loc lit
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
    | Trm_rel (loc, relop, lhs, rhs) ->
	let rec f loc relop lhs rhs =
	    let _loc = convert_loc loc in
	    match lhs with
	    | Trm_rel_left (lloc, lrelop, llhs, lrhs) ->
		let r = gen_apply2_idr env _loc relop lrhs rhs in
		let lr = f lloc lrelop llhs lrhs in
		<:expr< $lr$ && $r$ >>
	    | _ ->
		gen_apply2_idr env _loc relop lhs rhs in
	f loc relop lhs rhs
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
and gen_cases env _loc = List.map
    (fun (pat, cq) ->
	let pat' = gen_pattern env pat in
	let cq' = gen_expr env cq in
	<:match_case< $pat'$ -> $cq'$ >>)
and gen_apply2_idr env _loc (Idr op) x0 x1 =
    let x0' = gen_expr env x0 in
    let x1' = gen_expr env x1 in
    if op = "2o=" then <:expr< $x0'$ = $x1'$ >> else
    if op = "2o≠" then <:expr< $x0'$ <> $x1'$ >> else
    <:expr< $lid:op$ x0' x1' >>

let collect_inj def inj_map =
    match def with
    | Def_inj (loc, Idr injname, typ) ->
	let _loc = convert_loc loc in
	let rec flatten_types pts = function
	    | Trm_apply (_, Trm_apply (_, Trm_ref (_, c), pt), r)
	      when c = i_2o_arrow ->
		let (_, pt') = gen_ctyp pt in
		flatten_types (pt' :: pts) r
	    | Trm_ref (_, rt) -> (rt, List.rev pts)
	    | _ -> raise (Error (loc, "Invalid type experssion.")) in
	let (rt, pts) = flatten_types [] typ in
	let injs = try Idr_map.find rt inj_map with Not_found -> [] in
	let inj = <:ctyp< $uid:ctor_name injname$ of $list:pts$ >> in
	Idr_map.add rt (inj :: injs) inj_map
    | _ -> inj_map

let gen_val_def = function
    | Dec_lex _ -> ident
    | Dec_type (loc, typ) -> fun builder ->
	let _loc = convert_loc loc in
	let (idr, ctyp) = gen_ctyp typ in
	let ctyp' =
	    try
		let injs = Idr_map.find idr builder.Struct_builder.inj_map in
		<:ctyp< $ctyp$ == [ $list:injs$ ] >>
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
	    | Trm_ref (loc, f) ->
		(f, body')
	    | _ -> raise (Error (loc, "Invalid pattern in formals.")) in
	let (f, body'') = decons_formal body' pat in
	let binding' = <:binding< $lid:idr_to_string f$ = $body''$ >> in
	Struct_builder.add_def _loc f binding' At_val builder
    | Def_inj (loc, idr, typ) -> fun builder ->
	let _loc = convert_loc loc in
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
		let args' = List.map (fun v -> <:ident< $lid:v$ >>) parms in
		let body' = List.fold
		    (fun param body' -> <:expr< fun $lid:param$ -> $body'$ >>)
		    parms <:expr< $uid:c$ ($list:args'$) >> in
		<:binding< $lid:v$ = $body'$ >>
	    end in
	Struct_builder.add_def _loc idr binding' (At_inj r) builder

let gen_toplevel = function
    | Trm_where (loc, defs) ->
	let _loc = convert_loc loc in
	let inj_map = List.fold collect_inj defs Idr_map.empty in
	let builder = Struct_builder.create inj_map in
	let builder' = List.fold gen_val_def defs builder in
	Struct_builder.get_str_item _loc builder'
    | trm ->
	raise (Error (trm_location trm, "Expecting a structure."))
