(* Copyright 2010--2011  Petter Urkedal
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

open Ocamlbuild_pack
open Ocamlbuild_plugin

let ffoc1pp_path = "bin/ffoc1pp.native"

(* Ocamlbuild Plug-In for Fform/OC
 * =============================== *)

(** A modified version of [Ocaml_compiler.ocamlc_c] which adds an "-impl" option
    in front ofthe input file, so that ocamlc accepts a non-standard
    extension. *)
let custom_ocamlc_c tags arg out =
    let tags = tags ++ "ocaml" ++ "byte" in
    Cmd (S [!Options.ocamlc; A"-c"; T(tags ++ "compile");
	    Ocaml_utils.ocaml_ppflags tags;
	    Ocaml_utils.ocaml_include_flags arg;
	    A"-o"; Px out; A"-impl"; P arg])

(** A modified version of [Ocaml_compiler.ocamlopt_c] which adds an "-impl"
    option in front of the input file, so that ocamlopt accepts a non-standard
    extension. *)
let custom_ocamlopt_c tags arg out =
    let tags = tags ++ "ocaml" ++ "native" in
    Cmd (S [!Options.ocamlopt; A"-c"; Ocaml_arch.forpack_flags_of_pathname arg;
	    T(tags ++ "compile");
	    Ocaml_utils.ocaml_ppflags tags;
	    Ocaml_utils.ocaml_include_flags arg;
	    A"-o"; Px out; A"-impl"; P arg])

(** A version of [Ocaml_compiler.byte_compile_ocaml_interf] modified to use
    [custom_ocamlc_c]. *)
let custom_byte_compile_ocaml_interf ff cmi env build =
    let ff = env ff and cmi = env cmi in
    Ocaml_compiler.prepare_compile build ff;
    custom_ocamlc_c (tags_of_pathname ff ++ "interf") ff cmi

(** A modified version of [Ocaml_compiler.byte_compile_ocaml_implem] which uses
    [custom_ocamlc_c]. *)
let custom_byte_compile_ocaml_implem ?tag ff cmo env build =
    let ff = env ff and cmo = env cmo in
    Ocaml_compiler.prepare_compile build ff;
    custom_ocamlc_c (tags_of_pathname ff ++ "implem" +++ tag) ff cmo

(** A modified version of [Ocaml_compiler.native_compile_ocaml_implem] which
    uses [custom_ocamlopt_c]. *)
let custom_native_compile_ocaml_implem ?tag ?(cmx_ext = "cmx") ml env build =
    let ml = env ml in
    let cmi = Pathname.update_extensions "cmi" ml in
    let cmx = Pathname.update_extensions cmx_ext ml in
    Ocaml_compiler.prepare_link cmx cmi [cmx_ext; "cmi"] build;
    custom_ocamlopt_c (Tags.union (tags_of_pathname ml) (tags_of_pathname cmx)
		       ++ "implem" +++ tag) ml cmx

let ffoc1_compile_flags tags =
    if Tags.does_match tags (Tags.of_list ["ocamlstdlib"]) then N
    else A"-nostdlib"

let ffoc1_ocamlc_c tags ff out =
    let tags = tags ++ "ocaml" ++ "ffoc1pp" ++ "byte" in
    let include_flags = Ocaml_utils.ocaml_include_flags ff in
    let pp_flags =
	S[A ffoc1pp_path; include_flags; Flags.of_tags (tags ++ "pp")] in
    let pp_flags = Command.reduce pp_flags in
    Cmd (S [!Options.ocamlc; A"-c"; T(tags ++ "compile");
	    A"-pp"; Quote pp_flags; ffoc1_compile_flags tags; include_flags;
	    A"-o"; Px out; A"-impl"; P ff])

let ffoc1_ocamlopt_c tags ff out =
    let tags = tags ++ "ocaml" ++ "ffoc1pp" ++ "native" in
    let include_flags = Ocaml_utils.ocaml_include_flags ff in
    let pp_flags =
	S[A ffoc1pp_path; include_flags; Flags.of_tags (tags ++ "pp")] in
    let pp_flags = Command.reduce pp_flags in
    Cmd (S [!Options.ocamlopt; A"-c"; T(tags ++ "compile");
	    A"-pp"; Quote pp_flags; ffoc1_compile_flags tags; include_flags;
	    A"-o"; Px out; A"-impl"; P ff])

let byte_compile_ffoc1_interf ff cmi env build =
    let ff = env ff and cmi = env cmi in
    Ocaml_compiler.prepare_compile build ff;
    ffoc1_ocamlc_c (tags_of_pathname ff ++ "interf") ff cmi

let byte_compile_ffoc1_implem ?tag ff cmo env build =
    let ff = env ff and cmo = env cmo in
    Ocaml_compiler.prepare_compile build ff;
    ffoc1_ocamlc_c (tags_of_pathname ff ++ "implem" +++ tag) ff cmo

let native_compile_ffoc1_implem ?tag ff env build =
    let ff = env ff in
    let cmi = Pathname.update_extensions "cmi" ff in
    let cmx = Pathname.update_extensions "cmx" ff in
    Ocaml_compiler.prepare_link cmx cmi ["cmx"; "cmi"] build;
    ffoc1_ocamlopt_c
	(Tags.union (tags_of_pathname ff) (tags_of_pathname cmx) ++
	 "implem" +++ tag) ff cmx

(** Fform Stage 1 dependency analyzer. *)
let ffoc1dep arg out env build =
    let arg = env arg and out = env out in
    let tags = tags_of_pathname arg ++ "ffoc1pp" ++ "ocamldep" in
    Cmd(S[A ffoc1pp_path; T tags; A"--depend";
	  Ocaml_utils.ocaml_include_flags arg;
	  P arg; Sh ">"; Px out])

(** Fform Stage 1 preprocessor subcommand. *)
let ffoc1pp tag ff ff_ml env build =
    let ff = env ff and ff_ml = env ff_ml in
    let tags = tags_of_pathname ff ++ "ocaml" ++ "pp" ++ tag in
    let _ = Rule.build_deps_of_tags build tags in
    Cmd(S[A ffoc1pp_path; Ocaml_utils.ocaml_include_flags ff;
	  P ff; A"-o"; Px ff_ml])

;;
rule "Fform/OC Stage 1, Dependency Analysis"
    ~tags:["ocaml"; "pp"; "ffoc1pp"]
    ~prod:"%.ff.depends"
    ~deps:["%.ff"; ffoc1pp_path; "fflib/stdlex.ff"]
    (ffoc1dep "%.ff" "%.ff.depends");;

rule "ffoc1, byte compilation: ff -> cmo & cmi"
    ~tags:["ocaml"; "byte"; "pp"; "ffoc1pp"]
    ~prods:["%.cmo"; "%.cmi"]
    ~deps:["%.ff"; "%.ff.depends"; "fflib/stdlex.ff"]
    (byte_compile_ffoc1_implem "%.ff" "%.cmo");;

rule "ffoc1, native compilation: ff & cmi -> cmx & o"
    ~tags:["ocaml"; "native"; "pp"; "ffoc1pp"]
    ~prods:["%.cmx"; "%.o"]
    ~deps:["%.ff"; "%.ff.depends"; "%.cmi"; "fflib/stdlex.ff"]
    (native_compile_ffoc1_implem "%.ff");;

rule "ffoc1, preprocessing only: ff -> ff.ml"
    ~dep:"%.ff"
    ~prod:"%.ff.ml"
    (ffoc1pp "ff.ml" "%.ff" "%.ff.ml");;

flag ["ocaml"; "ffoc1pp"; "compile"] & A"-nopervasives";;
flag ["ocaml"; "ffoc1pp"; "link"] & A"-nopervasives";;
flag ["ocaml"; "ffoc1pp"; "pp"; "no_pervasive"] & A"--no-pervasive";;
flag ["ocamldep"; "ffoc1pp"; "no_pervasive"] & A"--no-pervasive";;
pflag ["ocaml"; "native"; "pack"] "for-pack"
    (fun param -> S [A "-for-pack"; A param]);;


(* Other Rules
 * =========== *)

let run_and_read = Ocamlbuild_pack.My_unix.run_and_read

let ocamlfind_query pkg =
    let s = run_and_read ("ocamlfind query " ^ pkg) in
    String.sub s 0 (String.length s - 1)

let ocaml_pkg lib =
    let tag = "use_" ^ lib in
    flag ["ocaml"; "compile"; tag] & S[A"-package"; A lib];
    flag ["ocaml"; "link";    tag] & S[A"-package"; A lib];
    flag ["ocaml"; "doc";     tag] & S[A"-package"; A lib]

let ocaml_pp lib pa =
    flag ["ocaml"; "pp"; pa]
	(S [A "-I"; A (ocamlfind_query lib); A (pa ^ ".cmo")])

let ffoc1pp_include path =
    flag ["ocaml"; "ffoc1pp"; "compile"]	& S[A"-I"; P path];
    flag ["ocaml"; "ffoc1pp"; "link"]		& S[A"-I"; P path];
    flag ["ocaml"; "ffoc1pp"; "pp"]		& S[A"-I"; P path];
    flag ["ocamldep"; "ffoc1pp"]		& S[A"-I"; P path]

let () = dispatch begin function
    | Before_options ->
	Options.use_ocamlfind := true;
	Options.use_menhir := true;
	()
    | After_rules ->
	flag ["ocaml"; "link"] & A"-linkpkg";
	flag ["ocaml"; "compile"; "use_threads"] & A"-thread";
	flag ["ocaml"; "link";    "use_threads"] & A"-thread";

	ocaml_pkg "oUnit";
	ocaml_pkg "sexplib";
	ocaml_pkg "menhirLib";
	ocaml_pkg "camomile";
	ocaml_pkg "camlp4";
	ocaml_pp "type-conv" "pa_type_conv";
	ocaml_pp "sexplib" "pa_sexp_conv";
	ocaml_lib "fflib";
	()
    | _ -> ()
end
