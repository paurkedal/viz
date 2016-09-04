(* Copyright (C) 2011--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Printf
open Diag
open Prereq
open Leaf_types
open Leaf_core
open Cst_types
open Cst_core
open Ast_types
open Ast_core

let header = "\
/* This file was generated by camlvizpp and will be overwritten. */

#define __STDC_LIMIT_MACROS 1
#define __STDC_CONSTANT_MACROS 1

#include <libvsl/cabi.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#ifndef CAMLparam6
#  define CAMLparam6(x0, x1, x2, x3, x4, x5) \\
	CAMLparam5 (x0, x1, x2, x3, x4); CAMLxparam1 (x5)
#endif
#ifndef CAMLparam7
#  define CAMLparam7(x0, x1, x2, x3, x4, x5, x6) \\
	CAMLparam5 (x0, x1, x2, x3, x4); CAMLxparam2 (x5, x6)
#endif
#ifndef CAMLparam8
#  define CAMLparam8(x0, x1, x2, x3, x4, x5, x6, x7) \\
	CAMLparam5 (x0, x1, x2, x3, x4); CAMLxparam3 (x5, x6, x7)
#endif
#ifndef CAMLparam9
#  define CAMLparam9(x0, x1, x2, x3, x4, x5, x6, x7, x8) \\
	CAMLparam5 (x0, x1, x2, x3, x4); CAMLxparam4 (x5, x6, x7, x8)
#endif

#define cviz_none Val_int(0)
value cviz_some(value x);

#define Voidp_val(x) *(void **)Data_custom_val(x)
value cviz_copy_ptr(void *);

value cviz_ustring_to_utf8(value x);
value cviz_copy_ustring(char const *);
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
  | Cti_custom of string * string * string option * bool
  | Cti_alias of atyp

type state = {
  st_cti_map : cti String_map.t;
  st_cprefix : string;
  st_function_prefix : string;
}

type conversion = {
  cv_is_opt : bool;
  cv_ctype : string;
  cv_prep_arg : string option;
  cv_conv_arg : string;
  cv_conv_res : string;
}

let is_ptr_path p =
  (* Recognising ptr unqualified is invasive, but this is only used for
   * modules defining C bindings, which and they should include C.memory. *)
  match Modpath.to_string_list p with
  | ["ptr"] | ["memory"; "ptr"] | ["C"; "memory"; "ptr"] -> true
  | _ -> false

let rec nonoption_conversion nparam state = function
  | Atyp_apply (_, Atyp_ref (Apath (_, p)), _) when is_ptr_path p ->
      ("void *", None, "Voidp_val", "cviz_copy_ptr")
  | Atyp_ref (Apath (loc, tpath)) when Modpath.is_atom tpath ->
      let tname = idr_to_string (Modpath.last_e tpath) in
      begin try
        match String_map.find tname state.st_cti_map with
        | Cti_custom (prefix, ctype, _, _) ->
            (ctype, None,
             sprintf "%s%s_of_value" prefix tname,
             sprintf "%scopy_%s" prefix tname)
        | Cti_alias tname' -> nonoption_conversion nparam state tname'
      with Not_found ->
      match tname with
      | "unit"   -> ("void", None, "Int_val",   "Val_int")
      | "bool"   -> ("int",  None, "Bool_val",  "Val_bool")
      | "int" | "size" | "offset" ->
          ("intnat",  None, "Long_val", "Val_long")
      | "nativeint" | "nint" | "nnat" ->
          ("nativeint", None, "Nativeint_val", "caml_copy_nativeint")
      | "int32"  -> ("int32", None, "Int32_val", "caml_copy_int32")
      | "int64"  -> ("int64", None, "Int64_val", "caml_copy_int64")
      | "float"  -> ("double", None, "Double_val", "caml_copy_double")
      | "octet"  -> ("char",  None, "Int_val",   "Val_int")
      | "utf8_string" ->
          ("char const *", None, "String_val", "caml_copy_string")
      | "string" ->
          ("char const *", Some "cviz_ustring_to_utf8", "String_val",
           "cviz_copy_ustring")
      | _ ->
          errf_at loc "Don't know how to pass values of type %s to \
                       C functions." tname
      end
  | Atyp_apply (loc, Atyp_apply (_, Atyp_ref (Apath (_, lens)), _), _)
        when (match List.rev (Modpath.to_string_list lens) with
              | "r" :: "lens" :: _
              | "R" :: "lens" :: _ -> true
              | _ -> false) ->
      ("void *", None, "vsl_lensv_ptr", "vsl_lensv_of_ptr")
  | Atyp_apply (loc, Atyp_ref (Apath (_, lens)), _)
        when (match List.rev (Modpath.to_string_list lens) with
              | "t" :: "lens" :: _ -> true
              | _ -> false) ->
      ("void *", None, "vsl_lensv_ptr", "vsl_lensv_of_ptr")
  | Atyp_apply (loc, Atyp_apply (_, Atyp_ref op_path, t),
                     Atyp_ref (Apath (tag_loc, tag_path)))
        when apath_eq_idr idr_2o_index op_path ->
      begin match Modpath.to_string_list tag_path with
      | ["b"] -> ("int", None, "Bool_val", "Val_bool")
      | ["i"] -> ("int", None, "Int_val",  "Val_int")
      | ["e"] -> ("int", None, "Int_val",  "Val_int") (* enum *)
      | ["l"] -> ("long", None, "Long_val", "Val_long")
      | ["v"] -> ("value", None, "", "")
      | _ -> errf_at tag_loc "Invalid conversion tag %s."
                     (Modpath.to_string tag_path)
      end
  | Atyp_apply (loc, x, Atyp_uvar _)
  | Atyp_apply (loc, x, Atyp_ref _) ->
      nonoption_conversion (nparam + 1) state x
  | t -> errf_at (atyp_loc t) "Unhandled type for the C ABI."

let conversion state t =
  let (is_opt, t) =
    match t with
    | Atyp_apply (_, Atyp_ref p_option, t)
            when apath_eq_string "option" p_option ->
       (true, t)
    | t -> (false, t) in
  let ctype, prep_arg, conv_arg, conv_res = nonoption_conversion 0 state t in
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

let output_cstub och v t cn_opt is_fin state =
  let cn = match cn_opt with Some cn -> cn | None -> avar_to_lid v in
  let stub_name = state.st_cprefix ^ (avar_name v) in
  let is_io, rt, ats = Ast_utils.flatten_arrows_for_c t in
  let (r, args) = List.fold
    begin fun at (i, args) ->
      (i + 1, (sprintf "x%d" i, conversion state at) :: args)
    end
    ats (0, []) in
  let args = List.rev args in
  output_char och '\n';
  fprintf och "CAMLprim value %s" stub_name;
  output_arglist och (fun (arg, _) -> fprintf och "value %s" arg) args;
  fprintf och "\n{\n\tCAMLparam%d " r;
  output_arglist och (output_string och <@ fst) args;
  output_string och ";\n";
  List.iter (output_arg_prep och) args;
  let is_unit =
    match rt with
    | Atyp_ref p_unit when apath_eq_string "unit" p_unit -> true
    | _ -> false in
  let output_call () =
    output_string och (state.st_function_prefix ^ cn);
    match t with
    | Atyp_arrow _ ->
        output_arglist och (output_arg och) args;
    | _ ->
        output_string och "()" in
  if is_unit then begin
    if is_fin then output_string och "\tif (Voidp_val(x0)) {\n\t";
    output_char och '\t';
    output_call ();
    output_string och ";\n";
    if is_fin then output_string och "\t\tVoidp_val(x0) = NULL;\n\t}\n";
    output_string och "\tCAMLreturn (Val_unit);\n";
  end else begin
    let rcv = conversion state rt in
    if rcv.cv_is_opt then begin
      fprintf och "\t%s y = " rcv.cv_ctype;
      output_call ();
      output_string och ";\n";
      fprintf och "\tCAMLreturn (y? cviz_some(%s(y)) : cviz_none);\n"
                  rcv.cv_conv_res
    end else begin
      fprintf och "\tCAMLreturn (%s(" rcv.cv_conv_res;
      output_call ();
      output_string och "));\n"
    end
  end;
  output_string och "}\n";
  if r > 5 then begin
    fprintf och "CAMLprim value %s_byte(value *argv, int argc)\n{\n\
                 \treturn %s(argv[0]"
            stub_name stub_name;
    for i = 1 to (r - 1) do fprintf och ", argv[%d]" i done;
    output_string och ");\n}\n";
  end;
  if is_fin then
    begin match t with
    | Atyp_arrow (loc, alab, ft, Atyp_apply (_, tc, Atyp_ref p_unit))
            when apath_eq_string "unit" p_unit ->
        let Apath (ftc_loc, ftc_p), ftparams = Ast_utils.atyp_unapply ft in
        if not (Modpath.is_atom ftc_p) then
            errf_at ftc_loc "Qualified names are not supported here.";
        let ftv = Modpath.last_e ftc_p in
        begin match tc with
        | Atyp_ref p_io when apath_eq_string "io" p_io -> ()
        | Atyp_apply (_, Atyp_ref (Apath (_, p_effect)), _)
                when Modpath.is_atom p_effect
                  && idr_is_effect_tycon (Modpath.last_e p_effect) -> ()
        | _ ->
            errf_at loc
                "Finalizer must return (io unit) or (effect φ unit)."
        end;
        begin try
          let ftn = idr_to_string ftv in
          match String_map.find ftn state.st_cti_map with
          | Cti_custom (cprefix, tname, None, true) ->
              let cti_map = String_map.add ftn
                  (Cti_custom (cprefix, tname, Some (avar_name v), true))
                  state.st_cti_map in
              {state with st_cti_map = cti_map}
          | Cti_custom (cprefix, tname, None, false) ->
              errf_at (avar_loc v)
                      "Cannot define finalizer for external type."
          | Cti_custom (cprefix, tname, Some _, _) ->
              errf_at (avar_loc v) "This type already has a finalizer."
          | Cti_alias _ ->
              errf_at (avar_loc v)
                      "Cannot define finalizer for a type alias."
        with Not_found ->
          errf_at loc "Finalizer must receive a C type."
        end
    | _ ->
        errf_at (atyp_loc t)
            "Finalizer must return (io unit) or (effect φ unit)."
    end
  else
    state

let declare_type_alias v origname state =
  { state with
    st_cti_map = String_map.add (avar_name v) (Cti_alias origname)
                                state.st_cti_map }

let declare_ctype och v ctype is_local state =
  let tname = avar_name v in
  if is_local then
    fprintf och "\n\
        #define %s%s_of_value(v) (*(%s*)Data_custom_val(v))\n\n\
        static struct custom_operations %s%s_ops;\n\n\
        value\n%scopy_%s(%s x)\n{\n\
        \tvalue v = caml_alloc_custom(&%s%s_ops, sizeof(%s), 0, 1);\n\
        \t%s%s_of_value(v) = x;\n\
        \treturn v;\n\
        }\n"
        state.st_cprefix tname ctype
        state.st_cprefix tname
        state.st_cprefix tname ctype
        state.st_cprefix tname ctype
        state.st_cprefix tname
  else
    fprintf och "\n\
      #define %s%s_of_value(v) (*(%s*)Data_custom_val(v))\n\n\
      value %scopy_%s(%s x);\n"
      state.st_cprefix tname ctype
      state.st_cprefix tname ctype;
  { state with
    st_cti_map = String_map.add tname
                    (Cti_custom (state.st_cprefix, ctype, None, is_local))
                    state.st_cti_map
  }

let output_ctype och cprefix sname tname = function
  | Cti_custom (cprefix, cname, vf_opt, true) ->
      let output_default gn = fprintf och "\tcustom_%s_default,\n" gn in
      fprintf och "\nstatic struct custom_operations %s%s_ops = {\n\
                   \t\"%s%s\",\n" cprefix tname sname tname;
      begin match vf_opt with
      | None -> output_default "finalize"
      | Some vf -> fprintf och "\t(void (*)(value))%s%s,\n" cprefix vf
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
     output_amod_c och mf @> output_amod_c och ma
  | Amod_lambda (_, _, _, m) | Amod_coercion (_, m, _)
  | Amod_suspend (_, m) | Amod_generate (_, m) ->
     output_amod_c och m
and output_adef_c och = function
  | Adef_use (_, x) ->
      begin match Ast_utils.interpret_use x with
      | `Stub_prefix pfx ->
          fun state -> {state with st_cprefix = pfx}
      | `Function_prefix pfx ->
          fun state -> {state with st_function_prefix = pfx}
      | `type_c (v, name) -> declare_ctype och v name false
      end
  | Adef_types defs ->
     List.fold
       begin function
       | (loc, v, ts, Atypinfo_cabi name) -> declare_ctype och v name true
       | (loc, v, ts, Atypinfo_alias name) -> declare_type_alias v name
       | _ -> ident
       end
       defs
  | Adef_cabi_val (loc, v, t, cn, valopts) ->
      if Ast_utils.atyp_is_const t || List.mem `Is_stub valopts then ident
      else output_cstub och v t cn (List.mem `Is_finalizer valopts)
  | Adef_include (loc, m) -> output_amod_c och m
  | Adef_in (loc, v, m) -> fun state ->
      let saved_cprefix = state.st_cprefix in
      let cprefix = state.st_cprefix ^ "_" ^ (avar_name v) ^ "_" in
      let state' = output_amod_c och m {state with st_cprefix = cprefix} in
      {state' with st_cprefix = saved_cprefix}
  | _ -> ident

let output_cstubs och sname m =
  output_string och header;
  Ast_utils.fold_amod_cabi_open
    (fun inc () -> fprintf och "#include <%s>\n" inc) m ();
  let state = {
    st_cti_map = String_map.empty;
    st_cprefix = "_cviz_";
    st_function_prefix = "";
  } in
  let state = output_amod_c och m state in
  String_map.iter (output_ctype och state.st_cprefix sname) state.st_cti_map
