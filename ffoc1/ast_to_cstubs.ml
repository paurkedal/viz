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

let header = "\
/* This file was generated by ffoc1 and will be overwritten. */

#define __STDC_LIMIT_MACROS 1
#define __STDC_CONSTANT_MACROS 1

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#define ffoc_none Val_int(0)
value ffoc_some(value x);

#define ffoc_ptr_of_value(x) *(void **)Data_custom_val(x)
value ffoc_copy_ptr(void *);

value ffoc_ustring_to_utf8(value x);
value ffoc_copy_ustring(char const *);
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

type cti =
    | Cti_custom of string * string option
    | Cti_alias of atyp

type state = {
    st_cti_map : cti String_map.t;
    st_stub_prefix : string;
}

type conversion = {
    cv_is_opt : bool;
    cv_ctype : string;
    cv_prep_arg : string option;
    cv_conv_arg : string;
    cv_conv_res : string;
}

let rec nonoption_conversion state = function
    | Atyp_apply (_, Atyp_ref (Apath (_, Avar (_, Idr "ptr"))), _) ->
	("void *", None, "ffoc_ptr_of_value", "ffoc_copy_ptr")
    | Atyp_ref (Apath ([], Avar (loc, Idr tname))) ->
	begin try
	    match String_map.find tname state.st_cti_map with
	    | Cti_custom (ctype, _) ->
		(ctype, None, sprintf "%s_of_value" tname,
		 sprintf "%scopy_%s" state.st_stub_prefix tname)
	    | Cti_alias tname' -> nonoption_conversion state tname'
	with Not_found ->
	match tname with
	| "unit"   -> ("void", None, "Int_val",   "Val_int")
	| "bool"   -> ("int",  None, "Bool_val",  "Val_bool")
	| "int"    -> ("int",  None, "Int_val",   "Val_int")
	| "nativeint" | "size" | "offset" ->
	    ("nativeint", None, "Nativeint_val", "caml_copy_nativeint")
	| "int32"  -> ("int32", None, "Int32_val", "caml_copy_int32")
	| "int64"  -> ("int64", None, "Int64_val", "caml_copy_int64")
	| "octet"  -> ("char",  None, "Int_val",   "Val_int")
	| "utf8"   -> ("char const *", None, "String_val", "caml_copy_string")
	| "string" ->
	    ("char const *", Some "ffoc_ustring_to_utf8", "String_val",
	     "ffoc_copy_ustring")
	| _ ->
	    errf_at loc "Don't know how to pass values of type %s to \
			 C functions." tname
	end
    | Atyp_apply (loc, Atyp_apply (_, Atyp_ref (Apath ([], op)), t),
		       Atyp_ref (Apath ([], Avar (tagloc, Idr tagname))))
	    when avar_idr op = idr_2o_index ->
	begin match tagname with
	| "b" -> ("int", None, "Bool_val", "Val_bool")
	| "i" -> ("int", None, "Int_val",  "Val_int")
	| "e" -> ("int", None, "Int_val",  "Val_int") (* enum *)
	| "l" -> ("long", None, "Long_val", "Val_long")
	| "v" -> ("value", None, "", "")
	| _ -> errf_at tagloc "Invalid conversion tag %s." tagname
	end
    | t -> errf_at (atyp_loc t) "Unhandled type for the C ABI."

let conversion state t =
    let (is_opt, t) =
	match t with
	| Atyp_apply (_, Atyp_ref (Apath ([], Avar (_, Idr "option"))), t) ->
	       (true, t)
	| t -> (false, t) in
    let (ctype, prep_arg, conv_arg, conv_res) = nonoption_conversion state t in
    {
	cv_is_opt = is_opt;
	cv_ctype = ctype;
	cv_prep_arg = prep_arg;
	cv_conv_arg = conv_arg;
	cv_conv_res = conv_res;
    }

let output_arg_prep och (v, cv) =
    if cv.cv_is_opt then
	fprintf och "\tvoid *%s_p;\n\
		     \tif (Is_block(%s)) {\n\
		     \t\t%s = Field(%s, 0);\n"
		     v v v v;
    Option.iter (fun f -> fprintf och "\t%s = %s(%s);\n" v f v) cv.cv_prep_arg;
    if cv.cv_is_opt then
	fprintf och "\t\t%s_p = %s(%s);\n\t}\n\
		     \telse\n\t\t%s_p = NULL;\n"
		     v cv.cv_conv_arg v v

let output_arg och (v, cv) =
    if cv.cv_is_opt then fprintf och "%s_p" v else
    fprintf och "%s(%s)" cv.cv_conv_arg v

let output_cstub och v t cname is_fin state =
    let is_io, rt, ats = Ast_utils.flatten_arrows_for_c t in
    let (r, args) = List.fold
	begin fun at (i, args) ->
	    (i + 1, (sprintf "x%d" i, conversion state at) :: args)
	end
	ats (0, []) in
    let args = List.rev args in
    output_char och '\n';
    fprintf och "CAMLprim value %s%s" state.st_stub_prefix
	    (Ast_core.avar_name v);
    output_arglist och (fun (arg, _) -> fprintf och "value %s" arg) args;
    fprintf och "\n{\n\tCAMLparam%d " r;
    output_arglist och (output_string och *< fst) args;
    output_string och ";\n";
    List.iter (output_arg_prep och) args;
    let is_unit =
       match rt with
       | Atyp_ref (Apath ([], Avar (loc, Idr "unit"))) -> true
       | _ -> false in
    let output_call () =
	output_string och cname;
	match t with
	| Atyp_arrow _ -> output_arglist och (output_arg och) args;
	| _ -> output_string och "()" in
    if is_unit then begin
	output_char och '\t';
	output_call ();
	output_string och ";\n\tCAMLreturn (Val_unit);\n"
    end else begin
	let rcv = conversion state rt in
	if rcv.cv_is_opt then begin
	    fprintf och "\t%s y = " rcv.cv_ctype;
	    output_call ();
	    output_string och ";\n";
	    fprintf och "\tCAMLreturn (y? ffoc_some(%s(y)) : ffoc_none);\n"
			rcv.cv_conv_res
	end else begin
	    fprintf och "\tCAMLreturn (%s(" rcv.cv_conv_res;
	    output_call ();
	    output_string och "));\n"
	end
    end;
    output_string och "}\n";
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
		match String_map.find ftn state.st_cti_map with
		| Cti_custom (tname, None) ->
		    let cti_map = String_map.add ftn
			    (Cti_custom (tname, Some (avar_name v)))
			    state.st_cti_map in
		    {state with st_cti_map = cti_map}
		| Cti_custom (tname, Some _) ->
		    errf_at (avar_loc v) "This type already has a finalizer."
		| Cti_alias _ ->
		    errf_at (avar_loc v)
			    "Cannot define finalizer for a type alias."
	    with Not_found ->
		errf_at loc "Finalizer must receive a C type."
	    end
	| _ ->
	    errf_at (atyp_loc t)
		"Finalizer must return (io unit) or (action φ unit)."
	end else
    state

let declare_type_alias v origname state =
    {state with
	st_cti_map = String_map.add (avar_name v) (Cti_alias origname)
				    state.st_cti_map}

let declare_ctype och v ctype state =
    let tname = avar_name v in
    fprintf och "\n\
	#define %s_of_value(v) (*(%s*)Data_custom_val(v))\n\n\
	static struct custom_operations %s_ops;\n\n\
	value\n%scopy_%s(%s x)\n{\n\
	\tvalue v = alloc_custom(&%s_ops, sizeof(%s), 0, 1);\n\
	\t%s_of_value(v) = x;\n\
	\treturn v;\n\
	}\n"
	tname ctype
	tname
	state.st_stub_prefix tname ctype
	tname ctype
	tname;
    {state with
	st_cti_map = String_map.add tname (Cti_custom (ctype, None))
		     state.st_cti_map
    }

let output_ctype och stub_prefix sname tname = function
    | Cti_custom (cname, vf_opt) ->
	let output_default gn = fprintf och "\tcustom_%s_default,\n" gn in
	fprintf och "\nstatic struct custom_operations %s_ops = {\n\
			\t\"%s%s\",\n" tname sname tname;
	begin match vf_opt with
	| None -> output_default "finalize"
	| Some vf -> fprintf och "\t(void (*)(value))%s%s,\n" stub_prefix vf
	end;
	output_default "compare";
	output_default "hash";
	output_default "serialize";
	output_default "deserialize";
	output_string och "};\n"
    | _ -> ()

let rec output_amod_c och = function
    | Amod_ref _ -> ident
    | Amod_defs (_, defs) -> List.fold (output_adef_c och) defs
    | Amod_apply (_, mf, ma) ->
       output_amod_c och mf *> output_amod_c och ma
    | Amod_lambda (_, _, s, m) | Amod_coercion (_, m, s) ->
       output_amod_c och m
and output_adef_c och = function
    | Adef_use (_, x) ->
	begin match Ast_utils.interpret_use x with
	| `Stub_prefix pfx ->
	    fun state -> {state with st_stub_prefix = pfx}
	end
    | Adef_types defs ->
       List.fold
	   begin function
	   | (loc, v, ts, Atypinfo_cabi name) -> declare_ctype och v name
	   | (loc, v, ts, Atypinfo_alias name) -> declare_type_alias v name
	   | _ -> ident
	   end
	   defs
    | Adef_cabi_val (loc, v, t, cn, valopts) ->
	if Ast_utils.atyp_is_const t || List.mem `Is_stub valopts then ident
	else output_cstub och v t cn (List.mem `Is_finalizer valopts)
    | Adef_include (loc, m) -> output_amod_c och m
    | _ -> ident

let output_cstubs och sname m =
    output_string och header;
    Ast_utils.fold_amod_cabi_open
	(fun inc () -> fprintf och "#include <%s>\n" inc) m ();
    let state = {st_cti_map = String_map.empty; st_stub_prefix = "_stub_"} in
    let state = output_amod_c och m state in
    String_map.iter (output_ctype och state.st_stub_prefix sname)
	state.st_cti_map
