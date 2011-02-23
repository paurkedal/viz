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

open Printf
open Diag
open FfPervasives
open Leaf_types
open Cst_types
open Cst_core
open Ast_types
open Ast_core

let header = "\
/* This file was generated by ffoc1 and will be overwritten. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
"

let rec output_arglist och ?(oparen = "(") ?(cparen = ")") ?(sep = ", ") f xs =
    output_string och oparen;
    begin match xs with
    | [] -> () | [x] -> f x
    | x :: xs ->
	f x;
	let rec loop = function
	    | [] -> ()
	    | x :: xs -> output_string och sep; f x; loop xs in
	loop xs
    end;
    output_string och cparen

let converters = function
    | Atyp_ref (Apath ([], Avar (loc, Idr tname))) ->
	begin match tname with
	| "bool"  -> ("Val_bool", "Bool_val")
	| "int"   -> ("Val_int", "Int_val")
	| "octet" -> ("Val_int", "Int_val")
	| _ ->
	    errf_at loc "Don't know how to pass values of type %s to \
			 C functions." tname
	end
    | Atyp_apply (loc, Atyp_apply (_, Atyp_ref (Apath ([], op)), t),
		       Atyp_ref (Apath ([], Avar (tagloc, Idr tagname))))
	    when avar_idr op = idr_2o_index ->
	begin match tagname with
	| "l" -> ("Val_long", "Long_val")
	| "i" -> ("Val_int", "Int_val")
	| "b" -> ("Val_bool", "Bool_val")
	| _ -> errf_at tagloc "Undefined C type tag %s." tagname
	end
    | t -> errf_at (atyp_loc t) "Unhandled type for the C ABI."

let output_arg och (v, at) =
    output_string och (snd (converters at));
    output_char och '(';
    output_string och v;
    output_char och ')'

let output_cstub och _ v t () =
    let is_io, rt, ats = Ast_utils.flatten_arrows_for_c t in
    let (r, rparams) = List.fold
	(fun at (i, rparams) -> (i + 1, (sprintf "v%d" i, at) :: rparams))
	ats (0, []) in
    let params = List.rev rparams in
    output_char och '\n';
    output_string och "CAMLprim value _stub_";
    output_string och (Ast_core.avar_name v);
    output_arglist och (fun (arg, _) -> fprintf och "value %s" arg) params;
    fprintf och "\n{\n";
    fprintf och "\tCAMLparam%d" r;
    output_arglist och (output_string och *< fst) params;
    fprintf och ";\n\tCAMLreturn(%s(" (fst (converters rt));
    output_string och (Ast_core.avar_name v);
    begin match t with
    | Atyp_arrow _ -> output_arglist och (output_arg och) params;
    | _ -> output_string och "()"
    end;
    output_string och "));\n}\n"

let output_include och path = fprintf och "#include <%s>\n" path

let output_cstubs och m =
    output_string och header;
    Ast_utils.fold_amod_cabi_open
	(fun inc () -> fprintf och "#include <%s>\n" inc) m ();
    Ast_utils.fold_amod_externals (output_cstub och) m [] ()
