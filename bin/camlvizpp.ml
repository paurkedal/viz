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
open Camlviz
open Diag
open Camlp4.PreCast
open Prereq
open Cst_types
open Ast_types
open Leaf_types

let usage = "camlvizpp [--print | -o OUTPUT] INPUT"

module String_set = Set.Make (String)
module Ocaml_printer = Camlp4.Printers.OCaml.Make(Camlp4.PreCast.Syntax)

let print_depend ~module_name just_modules topdir roots input_path m =
  let add_dep stra (Apath (_, p)) =
    let comps =
      match stra with
       | `Type | `Value | `Signature ->
          Modpath.to_string_list (Modpath.strip_last_e p)
       | `Structure ->
          Modpath.to_string_list p in
    match comps with
     | [] -> ident
     | x :: xs -> String_set.add x in
  let deps =
    Ast_utils.fold_amod_paths ~module_name add_dep m String_set.empty in
  let print_module_dep dep =
    print_char ' '; print_string (String.capitalize dep) in
  let check_and_print_dep ext dep =
    try
      let path = Parser.locate_source ~strip_ext:true ?topdir ~roots dep in
      print_char ' '; print_string (path ^ ".cmi");
      print_char ' '; print_string (path ^ ext)
    with Not_found -> () in
  if just_modules then begin
    print_string input_path; print_char ':';
    String_set.iter print_module_dep deps
  end else begin
    let p = Filename.chop_extension input_path in
    print_string (p ^ ".cmo"); print_string ": ";
    print_string (p ^ ".cmi");
    String_set.iter (check_and_print_dep ".cmo") deps;
    print_char '\n';
    print_string (p ^ ".cmx"); print_string ": ";
    print_string (p ^ ".cmi");
    String_set.iter (check_and_print_dep ".cmx") deps
  end;
  print_char '\n'

let print_cst term =
  let fo = Formatter.create () in
  Syn_print.print fo term;
  printf "%s\n" (Formatter.contents fo)

let print_ast amod =
  printf "%s\n" (Ast_utils.amod_to_string amod)

let pervasive_apath = Apath (Textloc.dummy, Modpath.atom (Idr "pervasive"))
let add_pervasive_in_asig = function
  | Asig_decs (loc, decs) ->
      Asig_decs (loc, Adec_open (Textloc.dummy, pervasive_apath) :: decs)
  | _ -> assert false
let rec add_pervasive_in_amod = function
  | Amod_defs (loc, defs) ->
      Amod_defs (loc, Adef_open (Textloc.dummy, pervasive_apath) :: defs)
  | Amod_coercion (loc, m, s) ->
      Amod_coercion (loc, add_pervasive_in_amod m, add_pervasive_in_asig s)
  | _ -> assert false

let _ =
  let opt_setter r x =
    if !r <> None then raise (Arg.Bad "Multiple values for option") else
    r := Some x in
  let in_path_opt = ref None in
  let out_path_opt = ref None in
  let do_cst = ref false in
  let do_ast = ref false in
  let do_depend = ref false in
  let do_cstubs = ref false in
  let do_consts = ref false in
  let add_loc = ref false in
  let open_pervasive = ref true in
  let rewrite_ast = ref true in
  let nroots = ref [] in
  let roots = ref [] in
  let strip_roots = ref [] in
  let topdir = ref None in
  let raw_deps = ref false in
  let serid = ref None in
  let optspecs = Arg.align [
    "-o", Arg.String (opt_setter out_path_opt),
        "PATH The output file.";
    "-N", Arg.String (fun p -> nroots := p :: !nroots),
        "PATH Prepend PATH to the root paths, but when combined with\n\
         --depend, don't create dependencies for it.";
    "-I", Arg.String (fun p -> roots := p :: !roots),
        "PATH Prepend PATH to the root paths to search for structures.";
    "-R", Arg.String (fun p -> strip_roots := p :: !strip_roots),
        "PATH Strip PATH from the input file path when determining \
         the module path.";
    "-T", Arg.String (fun p -> topdir := Some p),
        "PATH Add PATH prefix to all roots when scanning for dependencies \
         but don't include it in the result.";
    "--depend", Arg.Unit (fun () -> do_depend := true),
        " Output dependecies.";
    "--depend-modules",
        Arg.Unit (fun () -> do_depend := true; raw_deps := true),
        " Output all modules as dependencies, without checking paths.";
    "--cstubs", Arg.Unit (fun () -> do_cstubs := true),
        " Output C stubs, if any.";
    "--cstubs-serid", Arg.String (fun s -> serid := Some s),
        "STRING Prefix for serialization id of generated custom types.";
    "--consts", Arg.Unit (fun () -> do_consts := true),
        " Output C program to generate ML source defining constants.";
    "--cst", Arg.Unit (fun () -> do_cst := true),
        " Dump the concrete syntax tree.  Mainly for debugging.";
    "--ast", Arg.Unit (fun () -> do_ast := true),
        " Dump the abstract syntax tree.  Mainly for debugging.";
    "--no-ast-rewrite", Arg.Unit (fun () -> rewrite_ast := false),
        " Omit the standard AST rewrite.";
    "--no-pervasive", Arg.Unit (fun () -> open_pervasive := false),
        " Don't open the pervasive structure.";
    "--add-locations", Arg.Unit (fun () -> add_loc := true),
        " Add locations to O'Caml output.";
  ] in
  Arg.parse optspecs (opt_setter in_path_opt) usage;
  let require what = function
    | None ->
        printf "%s is required.\n" what;
        exit 64 (* EX_USAGE *)
    | Some x -> x in
  let in_path = require "An input path" !in_path_opt in
  let modpath = Modpath.of_string_list
          (String.split_on_char '/' (Filename.chop_extension in_path)) in
  let modpath =
    List.fold
      begin fun rootdir modpath' ->
        let rootpath =
          Modpath.of_string_list (String.split_on_char '/' rootdir) in
        match Modpath.strip_prefix rootpath modpath with
        | None -> modpath'
        | Some p -> p
      end
      !strip_roots modpath in
  let module_name = Filename.chop_extension (Filename.basename in_path) in
  try
    let with_out f =
      match !out_path_opt with
      | None -> f stdout
      | Some path -> let och = open_out path in f och; close_out och in
    let term = Parser.parse_file ~roots: (!roots @ !nroots) in_path in
    let term, () =
      Cst_rewrite.default_rewrite_ctrm
          Cst_rewrite.default_rewriter `Structure (term, ()) in
    if !do_cst then print_cst term else
    let amod = Cst_to_ast.build_amod term
            |> Bool.power !rewrite_ast Ast_rewrite.std_amod_rewrite
            |> Bool.power !open_pervasive add_pervasive_in_amod in
    if !do_ast then print_ast amod else
    if !do_cstubs then begin
      let serid =
        match !serid with
        | Some serid -> serid
        | None -> sprintf "org.vizlang.camlviz.%s." module_name in
      with_out (fun och -> Ast_to_cstubs.output_cstubs och serid amod)
    end else
    if !do_consts then
      with_out (fun och -> Ast_to_consts.output_consts och amod) else
    if !do_depend then
      print_depend ~module_name !raw_deps !topdir !roots in_path amod else
    let omod = Ast_to_p4.emit_toplevel ~modpath amod in
    if !add_loc then
      Ocaml_printer.print !out_path_opt
        (fun o -> o#set_loc_and_comments#implem)
        omod
    else
      Printers.OCaml.print_implem ?output_file:!out_path_opt omod
  with Error_at (loc, msg) ->
    eprintf "%s: %s\n" (Textloc.to_string loc) msg;
    exit 65 (* EX_DATAERR *)
