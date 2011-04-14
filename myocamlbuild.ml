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

let is_space = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false
let rec rskip_while f s i =
    if i > 0 && f (String.get s (i - 1)) then rskip_while f s (i - 1) else i
let split_on_space s =
    let rec loop j accu =
	if j = 0 then accu else
	let i = rskip_while (fun ch -> not (is_space ch)) s j in
	let accu = if i < j then String.sub s i (j - i) :: accu else accu in
	loop (rskip_while is_space s i) accu in
    loop (String.length s) []

let camlvizpp_path = "bin/camlvizpp.native"
let static = true

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

let camlviz_compile_flags tags =
    if Tags.does_match tags (Tags.of_list ["ocamlstdlib"]) then N
    else A"-nostdlib"

let camlviz_ocamlc_c tags ff out =
    let tags = tags ++ "ocaml" ++ "camlvizpp" ++ "byte" in
    let include_flags = Ocaml_utils.ocaml_include_flags ff in
    let pp_flags =
	S[A camlvizpp_path; include_flags; Flags.of_tags (tags ++ "pp")] in
    let pp_flags = Command.reduce pp_flags in
    Cmd (S [!Options.ocamlc; A"-c"; T(tags ++ "compile");
	    A"-pp"; Quote pp_flags; camlviz_compile_flags tags; include_flags;
	    A"-o"; Px out; A"-impl"; P ff])

let camlviz_ocamlopt_c tags ff out =
    let tags = tags ++ "ocaml" ++ "camlvizpp" ++ "native" in
    let include_flags = Ocaml_utils.ocaml_include_flags ff in
    let pp_flags =
	S[A camlvizpp_path; include_flags; Flags.of_tags (tags ++ "pp")] in
    let pp_flags = Command.reduce pp_flags in
    Cmd (S [!Options.ocamlopt; A"-c"; T(tags ++ "compile");
	    A"-pp"; Quote pp_flags; camlviz_compile_flags tags; include_flags;
	    A"-o"; Px out; A"-impl"; P ff])

let byte_compile_camlviz_interf ff cmi env build =
    let ff = env ff and cmi = env cmi in
    Ocaml_compiler.prepare_compile build ff;
    camlviz_ocamlc_c (tags_of_pathname ff ++ "interf") ff cmi

let byte_compile_camlviz_implem ?tag ff cmo env build =
    let ff = env ff and cmo = env cmo in
    Ocaml_compiler.prepare_compile build ff;
    camlviz_ocamlc_c (tags_of_pathname ff ++ "implem" +++ tag) ff cmo

let native_compile_camlviz_implem ?tag ff env build =
    let ff = env ff in
    let cmi = Pathname.update_extensions "cmi" ff in
    let cmx = Pathname.update_extensions "cmx" ff in
    Ocaml_compiler.prepare_link cmx cmi ["cmx"; "cmi"] build;
    camlviz_ocamlopt_c
	(Tags.union (tags_of_pathname ff) (tags_of_pathname cmx) ++
	 "implem" +++ tag) ff cmx

(** Fform Stage 1 dependency analyzer. *)
let camlvizdep arg out env build =
    let arg = env arg and out = env out in
    let tags = tags_of_pathname arg ++ "camlvizpp" ++ "ocamldep" in
    Cmd(S[P camlvizpp_path; T tags; A "--depend-modules"; A"-T"; P"..";
	  Ocaml_utils.ocaml_include_flags arg;
	  P arg; Sh ">"; Px out])

let camlvizcstubs arg out env build =
    let arg = env arg and out = env out in
    let tags = tags_of_pathname arg ++ "camlvizpp" ++ "cstubs" in
    Cmd(S[P camlvizpp_path; T tags; A "--cstubs";
	  Ocaml_utils.ocaml_include_flags arg;
	  P arg; Sh ">"; Px out])

let camlvizconsts arg out env build =
    let arg = env arg and out = env out in
    let tags = tags_of_pathname arg ++ "camlvizpp" ++ "consts" in
    Cmd(S[P camlvizpp_path; T tags; A "--consts";
	  Ocaml_utils.ocaml_include_flags arg;
	  P arg; Sh ">"; Px out])

let compile_fficgen arg out env build =
    let arg = env arg and out = env out in
    Cmd (S[A"cc"; A"-o"; Px out; A arg])

let runprog arg out env build =
    let arg = env arg and out = env out in
    Cmd (S[P arg; Sh ">"; Px out])

(** Fform Stage 1 preprocessor subcommand. *)
let camlvizpp tag ff ff_ml env build =
    let ff = env ff and ff_ml = env ff_ml in
    let tags = tags_of_pathname ff ++ "ocaml" ++ "pp" ++ tag in
    let _ = Rule.build_deps_of_tags build tags in
    Cmd(S[A camlvizpp_path; Ocaml_utils.ocaml_include_flags ff;
	  P ff; A"-o"; Px ff_ml])

;;
rule "Fform/OC Stage 1, Dependency Analysis"
    ~tags:["ocaml"; "pp"; "camlvizpp"]
    ~prod:"%.ff.depends"
    ~deps:[camlvizpp_path; "%.ff"; "fflib/stdlex.ff"]
    (camlvizdep "%.ff" "%.ff.depends");;

rule "camlviz, byte compilation: ff -> cmo & cmi"
    ~tags:["ocaml"; "byte"; "pp"; "camlvizpp"]
    ~prods:["%.cmo"; "%.cmi"]
    ~deps:["%.ff"; "%.ff.depends"; "fflib/stdlex.ff"]
    (byte_compile_camlviz_implem "%.ff" "%.cmo");;

rule "camlviz, native compilation: ff & cmi -> cmx & o"
    ~tags:["ocaml"; "native"; "pp"; "camlvizpp"]
    ~prods:["%.cmx"; "%.o"]
    ~deps:["%.ff"; "%.ff.depends"; "%.cmi"; "fflib/stdlex.ff"]
    (native_compile_camlviz_implem "%.ff");;

rule "camlviz, preprocessing only: ff -> ff.ml"
    ~deps:[camlvizpp_path; "%.ff"; "fflib/stdlex.ff"]
    ~prod:"%.ff.ml"
    (camlvizpp "ff.ml" "%.ff" "%.ff.ml");;

rule "camlviz, C stub generation: ff -> _FFIS.c"
    ~deps:[camlvizpp_path; "%.ff"; "fflib/stdlex.ff"]
    ~prod:"%_FFIS.c"
    (camlvizcstubs "%.ff" "%_FFIS.c");;

rule "camlviz, C program to emit ML source defining constants: ff -> %_FFICgen.c"
    ~deps:[camlvizpp_path; "%.ff"; "fflib/stdlex.ff"]
    ~prod:"%_FFICgen.c"
    (camlvizconsts "%.ff" "%_FFICgen.c");;

rule "camlviz, Run C program to emit constant defs: %_FFICgen -> _FFIC.ml"
    ~deps:["%_FFICgen"]
    ~prod:"%_FFIC.ml"
    (runprog "%_FFICgen" "%_FFIC.ml");;

rule "Compile and link the _FFICgen program"
    ~dep:"%_FFICgen.c"
    ~prod:"%_FFICgen"
    (compile_fficgen "%_FFICgen.c" "%_FFICgen");;

copy_rule "camlviz, Underscored module names to avoid conflict with O'Caml."
    "%.ff" "%_.ff";;

flag ["ocaml"; "camlvizpp"; "compile"] & A"-nopervasives";;
flag ["ocaml"; "camlvizpp"; "link"] & A"-nopervasives";;
flag ["ocaml"; "camlvizpp"; "pp"; "no_pervasive"] & A"--no-pervasive";;
flag ["ocamldep"; "camlvizpp"; "no_pervasive"] & A"--no-pervasive";;
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

let camlvizpp_include path =
    flag ["ocaml"; "camlvizpp"; "compile"]	& S[A"-I"; P path];
    flag ["ocaml"; "camlvizpp"; "link"]		& S[A"-I"; P path];
    flag ["ocaml"; "camlvizpp"; "pp"]		& S[A"-I"; P path];
    flag ["ocamldep"; "camlvizpp"]		& S[A"-I"; P path]

let llvm_libs () =
    let sL = run_and_read "llvm-config --ldflags" in
    let sl = run_and_read "llvm-config --libs jit interpreter native core" in
    S[S(List.map (fun l -> S[A"-ccopt"; A l]) (split_on_space sL));
      S(List.map (fun l -> S[A"-cclib"; A l]) (split_on_space sl));
      A"-cclib"; A"-lstdc++"]

let ocaml_cstubs name =
    flag ["link"; "ocaml"; "byte"; "use_lib" ^ name]
	 (S[A"-ccopt"; A"-L."; A"-cclib"; A("-l" ^ name);
	    A"-dllib"; A("-l" ^ name)]);
    flag ["link"; "ocaml"; "native"; "use_lib" ^ name]
	 (S[A"-ccopt"; A"-L."; A"-cclib"; A("-l" ^ name)]);
    dep ["ocaml"; "link"; "use_lib" ^ name] ["lib" ^ name ^ ".a"]

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
	ocaml_cstubs "fflib";
	ocaml_cstubs "tests";
	flag ["extension:c"; "compile"] & S[A"-ccopt"; A"-I.."];
	dep ["ocaml"; "compile"; "byte"; "use_fflib"] ["fflib.cma"];
	dep ["ocaml"; "compile"; "native"; "use_fflib"] ["fflib.cmxa"];
	flag ["link"; "ocaml"; "library"; "use_llvm_libs"] & llvm_libs ();
	if static then flag ["link"; "ocaml"; "byte"] (A"-custom");
	let fflib_includes = S[A"-I"; P"fflib"; A"-I"; P"compiler"] in
	flag ["ocaml"; "camlvizpp"] fflib_includes;
	flag ["cstubs"; "camlvizpp"] fflib_includes;
	flag ["ocamldep"; "camlvizpp"] & S[A"-N"; P"fflib"; A"-N"; P"compile"];
	()
    | _ -> ()
end
