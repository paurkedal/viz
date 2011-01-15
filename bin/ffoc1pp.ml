open Printf
open Ffoc1
open Diag
open Camlp4.PreCast

let usage = "ffoc1 [--print | -o OUTPUT] INPUT"

let _ =
    let opt_setter r x =
	if !r <> None then raise (Arg.Bad "Multiple values for option") else
	r := Some x in
    let in_path_opt = ref None in
    let out_path_opt = ref None in
    let do_print = ref false in
    let optspecs = Arg.align [
	"-o", Arg.String (opt_setter out_path_opt),
	    "PATH The output file.";
	"--print", Arg.Unit (fun () -> do_print := true),
	    " Print expression tree.";
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
	    Input.print fo term;
	    printf "%s\n" (Formatter.contents fo)
	end else begin
	    try
		let oc_ast = Gen_ocaml.gen_toplevel term in
		Printers.OCaml.print_implem ?output_file:!out_path_opt oc_ast
	    with Error_at (loc, msg) ->
		eprintf "%s: %s\n" (Location.to_string loc) msg;
		exit 65 (* EX_DATAERR *)
	end
    | None ->
	fprintf stderr "No result\n";
	exit 65 (* EX_DATAERR *)
