open Printf
open Ffoc1
open Diag
open Camlp4.PreCast
open FfPervasives
open Cst_types
open Ast_types
open Leaf_types

let usage = "ffoc1 [--print | -o OUTPUT] INPUT"

module String_set = Set.Make (String)

let print_depend input_path m =
    let rec extract comps = function
	| [] -> comps
	| Avar (_, Idr comp) :: vs -> extract (comp :: comps) vs in
    let add_dep stra th =
	let comps =
	    match stra with
	    | `Type | `Value | `Signature ->
		let Apath (vs, _) = th in extract [] vs
	    | `Structure ->
		let Apath (vs, v) = th in extract [] (v :: vs) in
	match comps with
	| [] -> ident
	| x :: xs -> String_set.add x in
    let deps = Ast_utils.fold_amod_paths add_dep m String_set.empty in
    print_string input_path;
    print_char ':';
    String_set.iter (fun c -> print_char ' '; print_string c) deps;
    print_char '\n'

let _ =
    let opt_setter r x =
	if !r <> None then raise (Arg.Bad "Multiple values for option") else
	r := Some x in
    let in_path_opt = ref None in
    let out_path_opt = ref None in
    let do_print = ref false in
    let do_depend = ref false in
    let use_ast = ref true in
    let optspecs = Arg.align [
	"-o", Arg.String (opt_setter out_path_opt),
	    "PATH The output file.";
	"--depend", Arg.Unit (fun () -> do_depend := true),
	    " Output dependecies.";
	"--print", Arg.Unit (fun () -> do_print := true),
	    " Print expression tree.";
	"--old-codegen", Arg.Unit (fun () -> use_ast := false),
	    " Use old code generator.";
    ] in
    Arg.parse optspecs (opt_setter in_path_opt) usage;
    let require what = function
	| None ->
	    printf "%s is required.\n" what;
	    exit 64 (* EX_USAGE *)
	| Some x -> x in
    let in_path = require "An input path" !in_path_opt in
    match Parser.parse_file in_path with
    | Some term ->
	if !do_print then begin
	    let fo = Formatter.create () in
	    Syn_print.print fo term;
	    printf "%s\n" (Formatter.contents fo)
	end else begin
	    try
		let amod = Cst_to_ast.build_amod term in
		if !do_depend then print_depend in_path amod else
		let omod = Ast_to_p4.emit_toplevel amod in
		Printers.OCaml.print_implem ?output_file:!out_path_opt omod
	    with Error_at (loc, msg) ->
		eprintf "%s: %s\n" (Location.to_string loc) msg;
		exit 65 (* EX_DATAERR *)
	end
    | None ->
	fprintf stderr "No result\n";
	exit 65 (* EX_DATAERR *)
