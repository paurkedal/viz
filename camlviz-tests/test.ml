open OUnit

let suite = "camlviz" >::: [
  Test_grammar.tests;
  Test_graphalgo.tests;
  Test_prereq.tests;
  Test_unicode.tests;
]

let () =
  let _ = run_test_tt_main suite in
  output_char stderr '\n'
