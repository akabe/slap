open OUnit

let suite =
  "ppx_slap" >::: [
    Test_vec.suite;
    Test_mat.suite;
  ]

let () = run_test_tt_main suite |> ignore
