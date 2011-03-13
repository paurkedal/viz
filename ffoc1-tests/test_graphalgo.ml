open OUnit
open Ffoc1
open FfPervasives
open Printf

module Vertex = struct
    type t = int
    let compare = compare
end

module Graph = struct
    type vertex = int
    type graph = int list array

    module Vertex_map = Map.Make (Vertex)

    let fold_adjacent g f i =
	List.fold f (Array.get g i)
end

module GA = Graphalgo.ST_algo (Graph)
module VSet = Set.Make (Vertex)

let dump_graph g =
    eprintf "Input:\n";
    Array.iteri begin fun i xs ->
	eprintf "    %d:" i;
	List.iter (fun x -> eprintf " %d" x) xs;
	eprintf "\n"
    end g
let dump_components comps =
    eprintf "Components:\n";
    List.iter begin fun xs ->
	eprintf "    *";
	List.iter (fun x -> eprintf " %d" x) xs;
	eprintf "\n"
    end comps;
    flush stderr

let check_strongly_connected g comps =
    let seen = List.fold
	begin fun vs seen ->
	    List.iter (fun v ->
		assert_bool "Unseen vertex." (not (VSet.mem v seen))) vs;
	    let seen = List.fold VSet.add vs seen in
	    List.iter begin fun v ->
		List.iter begin fun i ->
		    assert_bool "Seen vertex." (VSet.mem i seen)
		end g.(v)
	    end vs;
	    seen
	end comps VSet.empty in
    assert_bool "Seen all." (VSet.cardinal seen = Array.length g)

let test_fold_strongly_connected () =
    let g = [|
	(*  0: *) [1; 3];
	(*  1: *) [0; 3];
	(*  2: *) [1];
	(*  3: *) [7; 11];
	(*  4: *) [5; 6];
	(*  5: *) [];
	(*  6: *) [5];
	(*  7: *) [0; 9];
	(*  8: *) [8; 9];
	(*  9: *) [8; 10];
	(* 10: *) [];
	(* 11: *) [];
	(* 12: *) [1; 13];
	(* 13: *) [12];
    |] in
    let vs = List.init (Array.length g) ident in
    let comps = GA.fold_strongly_connected g List.push vs [] in
    let comps = List.rev comps in
    dump_components comps;
    check_strongly_connected g comps;
    for i = 0 to 1999 do
	let n = Random.int (1 lsl (Random.int 6)) in
	let g = Array.init n
	    (fun _ -> List.init (Random.int n) (fun _ -> Random.int n)) in
	let vs = List.init (Array.length g) ident in
	let comps = GA.fold_strongly_connected g List.push vs [] in
	let comps = List.rev comps in
	try
	    check_strongly_connected g comps
	with Failure s ->
	    dump_graph g;
	    dump_components comps;
	    assert_failure s
    done

let tests = "graphalgo" >::: [
    "fold_strongly_connected" >:: test_fold_strongly_connected;
]
