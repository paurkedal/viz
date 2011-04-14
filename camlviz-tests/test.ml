open OUnit

let suite = "fform" >::: [
    Test_grammar.tests;
    Test_graphalgo.tests;
    Test_pervasives.tests;
    Test_unicode.tests;
]

let () =
    let _ = run_test_tt_main suite in
    output_char stderr '\n'
