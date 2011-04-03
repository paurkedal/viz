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
open Leaf_core
open Cst_types
open Cst_core
open Ast_types
open Ast_core

type cti = {
    cti_ctype : string;
    cti_finalize : string option;
}

let header = "\
/* This file was generated by ffoc1 and will be overwritten. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>

#define ffoc_none Val_int(0)
value ffoc_some(value x);
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

let converters cti_map = function
    | Atyp_apply (_,
	    Atyp_ref (Apath ([], Avar (_, Idr "option"))),
	    Atyp_ref (Apath ([], Avar (loc, Idr tname)))) ->
	if String_map.mem tname cti_map then
	    (sprintf "%s_to_option" tname, sprintf "%s_of_option" tname) else
	errf_at loc "Invalid C type or option not handled for this C type."
    | Atyp_ref (Apath ([], Avar (loc, Idr tname))) ->
	if String_map.mem tname cti_map then
	    (sprintf "%s_to_value" tname, sprintf "%s_of_value" tname) else
	begin match tname with
	| "bool"  -> ("Val_bool", "Bool_val")
	| "int"   -> ("Val_int", "Int_val")
	| "octet" -> ("Val_int", "Int_val")
	| "utf8" -> ("caml_copy_string", "String_val")
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

let output_arg och cti_map (v, at) =
    output_string och (snd (converters cti_map at));
    output_char och '(';
    output_string och v;
    output_char och ')'

let output_cstub och v t cname is_fin cti_map =
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
    let is_unit =
       match rt with
       | Atyp_ref (Apath ([], Avar (loc, Idr "unit"))) -> true
       | _ -> false in
    output_string och ";\n\t";
    if not is_unit then
       fprintf och "CAMLreturn (%s(" (fst (converters cti_map rt));
    output_string och cname;
    begin match t with
    | Atyp_arrow _ -> output_arglist och (output_arg och cti_map) params;
    | _ -> output_string och "()"
    end;
    if is_unit then output_string och ";\n\tCAMLreturn (Val_unit);\n}\n"
	       else output_string och "));\n}\n";
    if is_fin then
       begin match t with
       | Atyp_arrow (loc, Atyp_ref (Apath ([], ftv)),
	       Atyp_apply (_, tc,
		   Atyp_ref (Apath ([], Avar (_, Idr "unit"))))) ->
	   begin match tc with
	   | Atyp_ref (Apath ([], Avar (_, Idr "io")))
	   | Atyp_apply (_,
		   Atyp_ref (Apath ([], Avar (_, Idr "action"))), _) ->
	       ()
	   | _ ->
	       errf_at loc
		   "Finalizer must return (io unit) or (action φ unit)."
	   end;
	   begin try
	       let ftn = avar_name ftv in
	       let cti = String_map.find ftn cti_map in
	       String_map.add ftn {cti with cti_finalize = Some (avar_name v)}
		   cti_map
	   with Not_found ->
	       errf_at loc "Finalizer must receive a C type."
	   end
       | _ ->
	   errf_at (atyp_loc t)
	       "Finalizer must return (io unit) or (action φ unit)."
       end else
    cti_map

let declare_ctype och v ctype =
    let tname = avar_name v in
    fprintf och "\n\
	#define %s_of_value(v) (*(%s*)Data_custom_val(v))\n\n\
	static struct custom_operations %s_ops;\n\n\
	static value %s_to_value(%s x)\n{\n\
	\    value v = alloc_custom(&%s_ops, sizeof(%s), 0, 1);\n\
	\    %s_of_value(v) = x;\n\
	\    return v;\n\
	}\n\
	#define %s_to_option(x) ((x)? ffoc_some(%s_to_value(x)) : ffoc_none)\n"
	tname ctype tname tname ctype tname ctype tname tname tname;
    String_map.add tname {
       cti_ctype = ctype;
       cti_finalize = None;
    }

let output_ctype och sname tname cti =
    let output_default gn = fprintf och "\tcustom_%s_default,\n" gn in
    fprintf och "\nstatic struct custom_operations %s_ops = {\n\
		    \t\"%s%s\",\n" tname sname tname;
    begin match cti.cti_finalize with
    | None -> output_default "finalize"
    | Some vf -> fprintf och "\t(void (*)(value))_stub_%s,\n" vf
    end;
    output_default "compare";
    output_default "hash";
    output_default "serialize";
    output_default "deserialize";
    output_string och "};\n"

let rec output_amod_c och = function
    | Amod_ref _ -> ident
    | Amod_defs (_, defs) -> List.fold (output_adef_c och) defs
    | Amod_apply (_, mf, ma) ->
       output_amod_c och mf *> output_amod_c och ma
    | Amod_lambda (_, _, s, m) | Amod_coercion (_, m, s) ->
       output_amod_c och m
and output_adef_c och = function
    | Adef_types defs ->
       List.fold
	   begin function
	   | (loc, v, ts, Atypinfo_cabi name) -> declare_ctype och v name
	   | _ -> ident
	   end
	   defs
    | Adef_cabi_val (loc, v, t, cn, is_fin) -> output_cstub och v t cn is_fin
    | Adef_include (loc, m) -> output_amod_c och m
    | _ -> ident

let output_cstubs och sname m =
    output_string och header;
    Ast_utils.fold_amod_cabi_open
       (fun inc () -> fprintf och "#include <%s>\n" inc) m ();
    let cti_map = output_amod_c och m String_map.empty in
    String_map.iter (output_ctype och sname) cti_map
