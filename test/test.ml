open OUnit

let suite =
  "Slap" >:::
    [Test_vec.suite;
     Test_mat.suite]

let _ =
  run_test_tt_main suite
