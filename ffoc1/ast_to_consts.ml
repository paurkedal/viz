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

open Diag
open Leaf_types
open Ast_types
open Ast_core
open Printf
open FfPervasives

let output_const och v t cx =
    match t with
    | Atyp_ref (Apath ([], Avar (loc, Idr tn))) ->
	fprintf och "\tprintf(\"let %s = \"); " (avar_name v);
	begin match tn with
	| "bool" -> fprintf och "fputs(%s? \"true\" : \"false\", stdout)" cx
	| "int" -> fprintf och "printf(\"%%d\", %s)" cx
	| "utf8" -> fprintf och "fputq(%s, stdout)" cx
	  (* TODO: Fix string quoting. *)
	| _ -> errf_at (atyp_loc t) "Unsupported type for C constant."
	end;
	output_string stdout "; fputc('\\n', stdout);\n"
    | _ ->
	errf_at (atyp_loc t) "Unsupported type for C constant."

let rec output_inj_check och i = function
    | [] -> ()
    | (loc, v, t, Ainjnum_cabi cn) :: injs ->
	begin match t with
	| Atyp_arrow (loc, _, _) ->
	    errf_at loc "A constant type is required for C enum."
	| _ -> ()
	end;
	fprintf stdout "\tck_enum(%d, %s, \"%s\", \"%s\");\n"
	    i cn (Location.to_string loc) (avar_name v);
	output_inj_check och (i + 1) injs
    | (loc, v, t, Ainjnum_auto) :: injs ->
	output_inj_check och (i + 1) injs

let rec output_amod_c och = function
    | Amod_ref _ -> ()
    | Amod_defs (_, defs) -> List.iter (output_adef_c och) defs
    | Amod_apply (_, mf, ma) ->
	output_amod_c och mf;
	output_amod_c och ma
    | Amod_lambda (_, _, s, m) | Amod_coercion (_, m, s) ->
	output_amod_c och m
and output_adef_c och = function
    | Adef_cabi_val (loc, v, t, cx, valopts) ->
	if Ast_utils.atyp_is_const t then output_const och v t cx
    | Adef_types cases ->
	List.iter
	    begin function
		| (_, _, _, Atypinfo_injs injs) -> output_inj_check och 0 injs
		| _ -> ()
	    end cases
    | Adef_include (loc, m) -> output_amod_c och m
    | _ -> ()

let output_consts och m =
    output_string och "
#define __STDC_LIMIT_MACROS 1
#define __STDC_CONSTANT_MACROS 1
#include <stdio.h>
#include <stdlib.h>
";
    Ast_utils.fold_amod_cabi_open
	(fun inc () -> fprintf och "#include <%s>\n" inc) m ();
    output_string och "

static void
fputq(char *s, FILE *out)
{
    fputc('\"', out);
    while (*s) {
	switch (*s) {
	    case '\"': case '\\\\': fputc('\\\\', out); break;
	}
	fputc(*s++, out);
    }
    fputc('\"', out);
}

static void
ck_enum(int i_ml, int i_c, char const *loc, char const *name_ml)
{
    if (i_ml != i_c) {
	fprintf(stderr,
		\"%s: inj %s of value %d does not equal the value %d of the \"
		\"C enum constant.\", loc, name_ml, i_ml, i_c);
	exit(65);
    }
}

int main()
{
";
    output_amod_c och m;
    output_string och "\treturn 0;\n}\n"
