open OUnit
open CamomileLibrary.Default.Camomile
open Ffoc1.Unicode

let test_helpers () =
    assert (UChar.is_idrchr (UChar.of_char 'a'));
    assert (UChar.is_idrchr (UChar.of_char '_'));
    assert (not (UChar.is_idrchr (UChar.of_char '-')));
    ()

let tests = "unicode" >::: [
    "helpers" >:: test_helpers;
]
