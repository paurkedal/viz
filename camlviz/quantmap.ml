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

open FfPervasives
open Ast_types
open Ast_core
open Leaf_types

module Atyp = struct
    type t = avar list * atyp
    let compare (alphas, t) (betas, u) =
	let nalphas, nbetas = List.length alphas, List.length betas in
	if nalphas < nbetas then -1 else
	if nalphas > nbetas then 1 else
	let t, u =
	    List.fold2
		begin fun (alpha, beta) (t, u) ->
		    let gamma = fresh_type_avar () in
		    (atyp_subst alpha gamma t, atyp_subst beta gamma u)
		end
		(alphas, betas)
		(t, u) in
	atyp_compare t u
end

module Type_map = Map.Make(Atyp)

type t = Modpath.t Type_map.t

let empty = Type_map.empty
let card = Type_map.cardinal
let add = Type_map.add
let find k m = try Some (Type_map.find k m) with Not_found -> None

let open_module qn m =
    let strip_prefix p (Apath (loc, q) as ap) =
	match Modpath.strip_prefix p q with
	| None -> ap
	| Some q' -> Apath (loc, q') in
    let f (alphas, t) e =
	let t' = atyp_map ~on_apath:(strip_prefix qn) t in
	if atyp_compare t t' = 0 then ident else
	Type_map.add (alphas, t') e in
    Type_map.fold f m m

let load ~roots fpath =
    let ich = open_in fpath in
    let lnno = ref (-1) in
    let rec collect m =
	try
	    lnno := !lnno + 1;
	    let ln = input_line ich in
	    let i0 = String.skip_while Char.is_space ln 0 in
	    if i0 == String.length ln || ln.[i0] == '#' then collect m else
	    let i1 = String.skip_while (not *< Char.is_space) ln i0 in
	    let i2 = String.skip_while Char.is_space ln i1 in
	    let s_dfm = String.sub ln i0 (i1 - i0) in
	    let p_dfm = Modpath.of_string s_dfm in
	    let s_dfs = String.after i2 ln in
	    let locb = Location.Bound.init_lc fpath !lnno i2 in
	    let e_dfs = Parser.parse_string ~roots ~locb s_dfs in
	    let t_dfs = Cst_to_ast.build_atyp e_dfs in
	    collect (add (atyp_to_ascm t_dfs) p_dfm m)
	with
	| End_of_file -> close_in ich; m
	| xc ->          close_in ich; raise xc in
    collect

let load_all ~roots =
    List.fold
	begin fun root ->
	    let fp = Filename.concat root "compat/quant.map" in
	    if Sys.file_exists fp then load ~roots fp else ident
	end
	roots

let filter = Type_map.filter

let filter_onelevel mp =
    filter (fun _ mp' -> Modpath.compare mp (Modpath.strip_last_e mp') = 0)

let filter_subhier mp = filter (fun _ -> Modpath.has_prefix mp)

let fold = Type_map.fold
