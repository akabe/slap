open OUnit

let suite =
  "Slap" >:::
    [Test_vec.suite]

let _ =
  run_test_tt_main suite
