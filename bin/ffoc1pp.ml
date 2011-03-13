open Printf
open Ffoc1
open Diag
open Camlp4.PreCast
open FfPervasives
open Cst_types
open Ast_types
open Leaf_types

let usage = "ffoc1pp [--print | -o OUTPUT] INPUT"

module String_set = Set.Make (String)

let print_depend roots input_path m =
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
    let check_and_print_dep dep =
	try
	    let path = Parser.locate_source ~strip_ext:true ~roots dep in
	    print_char ' '; print_string path
	with Not_found -> () in
    print_string input_path;
    print_char ':';
    String_set.iter check_and_print_dep deps;
    print_char '\n'

let print_cst term =
    let fo = Formatter.create () in
    Syn_print.print fo term;
    printf "%s\n" (Formatter.contents fo)

let print_ast amod =
    printf "%s\n" (Ast_utils.amod_to_string amod)

let pervasive_apath = Apath ([], Avar (Location.dummy, Idr "pervasive"))
let add_pervasive_in_asig = function
    | Asig_decs (loc, decs) ->
	Asig_decs (loc, Adec_open (Location.dummy, pervasive_apath) :: decs)
    | _ -> assert false
let rec add_pervasive_in_amod = function
    | Amod_defs (loc, defs) ->
	Amod_defs (loc, Adef_open (Location.dummy, pervasive_apath) :: defs)
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
    let open_pervasive = ref true in
    let nroots = ref [] in
    let roots = ref [] in
    let optspecs = Arg.align [
	"-o", Arg.String (opt_setter out_path_opt),
	    "PATH The output file.";
	"-N", Arg.String (fun p -> nroots := p :: !nroots),
	    "PATH Prepend PATH to the root paths, but when combined with\n\
	     --depend, don't create dependencies for it.";
	"-I", Arg.String (fun p -> roots := p :: !roots),
	    "PATH Prepend PATH to the root paths to search for structures.";
	"--depend", Arg.Unit (fun () -> do_depend := true),
	    " Output dependecies.";
	"--cstubs", Arg.Unit (fun () -> do_cstubs := true),
	    " Output C stubs, if any.";
	"--cst", Arg.Unit (fun () -> do_cst := true),
	    " Dump the concrete syntax tree.  Mainly for debugging.";
	"--ast", Arg.Unit (fun () -> do_ast := true),
	    " Dump the abstract syntax tree.  Mainly for debugging.";
	"--no-pervasive", Arg.Unit (fun () -> open_pervasive := false),
	    " Don't open the pervasive structure.";
    ] in
    Arg.parse optspecs (opt_setter in_path_opt) usage;
    let require what = function
	| None ->
	    printf "%s is required.\n" what;
	    exit 64 (* EX_USAGE *)
	| Some x -> x in
    let in_path = require "An input path" !in_path_opt in
    match Parser.parse_file ~exts: [""] ~roots: (!roots @ !nroots) in_path with
    | Some term ->
	begin try
	    let term, () =
		Cst_rewrite.default_rewrite_ctrm
		    Cst_rewrite.default_rewriter `Structure (term, ()) in
	    if !do_cst then print_cst term else
	    let amod = Cst_to_ast.build_amod term in
	    let amod = if !open_pervasive then add_pervasive_in_amod amod
		       else amod in
	    if !do_ast then print_ast amod else
	    if !do_cstubs then Ast_to_cstubs.output_cstubs stdout amod else
	    if !do_depend then print_depend !roots in_path amod else
	    let omod = Ast_to_p4.emit_toplevel amod in
	    Printers.OCaml.print_implem ?output_file:!out_path_opt omod
	with Error_at (loc, msg) ->
	    eprintf "%s: %s\n" (Location.to_string loc) msg;
	    exit 65 (* EX_DATAERR *)
	end
    | None ->
	fprintf stderr "No result\n";
	exit 65 (* EX_DATAERR *)
