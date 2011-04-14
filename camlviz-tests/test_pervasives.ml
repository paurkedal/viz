open OUnit
open Camlviz.FfPervasives

let test_list () =
    let xs = List.init 5 ident in
    assert (List.split_before ((=) 2) xs = ([0; 1], [2; 3; 4]));
    assert (List.split_after ((=) 2) xs = ([0; 1; 2], [3; 4]));
    assert (List.drop_while (fun x -> x <= 2) xs = [3; 4]);
    ()

let tests = "pervasives" >::: [
    "list" >:: test_list;
]
