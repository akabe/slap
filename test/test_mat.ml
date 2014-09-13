(* test_mat.ml *)

open Format
open OUnit
open Slap.Size
open Slap.Mat
open Bigarray

let m_00  = init_cols int zero zero (fun _ _ -> 42)
let m_0n  = init_cols int zero four (fun _ _ -> 42)
let m_n0  = init_cols int four zero (fun _ _ -> 42)
let m_ord = init_cols int four five (fun i j -> 10 * i + j)
let m_sub = submat_dyn two three ~ar:2 ~ac:2 m_ord

(* test of Slap.Mat.to_array *)
let test_to_array () =
  "m_00"  @? (to_array m_00  = [||]);
  "m_0n"  @? (to_array m_0n  = [||]);
  "m_n0"  @? (to_array m_n0  = [|[||];[||];[||];[||]|]);
  "m_ord" @? (to_array m_ord = [|[|11;12;13;14;15|];
                                 [|21;22;23;24;25|];
                                 [|31;32;33;34;35|];
                                 [|41;42;43;44;45|]|]);
  "m_sub" @? (to_array m_sub = [|[|22;23;24|];
                                 [|32;33;34|]|])

(* test of Slap.Mat.to_list *)
let test_to_list () =
  "m_00"  @? (to_list m_00  = []);
  "m_0n"  @? (to_list m_0n  = []);
  "m_n0"  @? (to_list m_n0  = [[];[];[];[]]);
  "m_ord" @? (to_list m_ord = [[11;12;13;14;15];
                               [21;22;23;24;25];
                               [31;32;33;34;35];
                               [41;42;43;44;45]]);
  "m_sub" @? (to_list m_sub = [[22;23;24];
                               [32;33;34]])

(* test of Slap.Mat.of_array_dyn *)
let test_of_array_dyn () =
  let of_array_dyn m n a = of_array_dyn int m n a in
  let (=) x y = (to_list x = to_list y) in
  "m_00"  @? (of_array_dyn zero zero [||] = m_00);
  "m_0n"  @? (of_array_dyn zero four [||] = m_0n);
  "m_n0"  @? (of_array_dyn four zero [|[||];[||];[||];[||]|] = m_n0);
  "m_ord" @? (of_array_dyn four five [|[|11;12;13;14;15|];
                                       [|21;22;23;24;25|];
                                       [|31;32;33;34;35|];
                                       [|41;42;43;44;45|]|] = m_ord);
  let (@!) msg f =
    assert_raises ~msg (Invalid_argument "Slap.Mat.of_array_dyn") f
  in
  "exn/rows" @! (fun () -> of_array_dyn two three [|[|11;12|];
                                                    [|21;22|]|]);
  "exn/cols" @! (fun () -> of_array_dyn two three [|[|11;12;13|]|]);
  "exn/rect" @! (fun () -> of_array_dyn two three [|[|11;12;13|];
                                                    [|21;22|]|])

(* test of Slap.Mat.of_list_dyn *)
let test_of_list_dyn () =
  let of_list_dyn m n l = of_list_dyn int m n l in
  let (=) x y = (to_array x = to_array y) in
  "m_00"  @? (of_list_dyn zero zero [] = m_00);
  "m_0n"  @? (of_list_dyn zero four [] = m_0n);
  "m_n0"  @? (of_list_dyn four zero [[];[];[];[]] = m_n0);
  "m_ord" @? (of_list_dyn four five [[11;12;13;14;15];
                                     [21;22;23;24;25];
                                     [31;32;33;34;35];
                                     [41;42;43;44;45]] = m_ord);
  let (@!) msg f =
    assert_raises ~msg (Invalid_argument "Slap.Mat.of_list_dyn") f
  in
  "exn/rows" @! (fun () -> of_list_dyn two three [[11;12];
                                                  [21;22]]);
  "exn/cols" @! (fun () -> of_list_dyn two three [[11;12;13]]);
  "exn/rect" @! (fun () -> of_list_dyn two three [[11;12;13];
                                                  [21;22]])

(* test of Mat.fold_lefti *)
let test_fold_lefti () =
  let fli a =
    fold_lefti (fun i acc cv -> (i, Slap.Vec.to_list cv) :: acc) [] a
  in
  "m_00"  @? (fli m_00  = []);
  "m_0n"  @? (fli m_0n  = [(4, []);
                           (3, []);
                           (2, []);
                           (1, [])]);
  "m_n0"  @? (fli m_n0  = []);
  "m_ord" @? (fli m_ord = [(5, [15;25;35;45]);
                           (4, [14;24;34;44]);
                           (3, [13;23;33;43]);
                           (2, [12;22;32;42]);
                           (1, [11;21;31;41])]);
  "m_sub" @? (fli m_sub = [(3, [24; 34]);
                           (2, [23; 33]);
                           (1, [22; 32])])

(* test of Mat.fold_righti *)
let test_fold_righti () =
  let fri a =
    fold_righti (fun i cv acc -> (i, Slap.Vec.to_list cv) :: acc) a []
  in
  "m_00"  @? (fri m_00  = []);
  "m_0n"  @? (fri m_0n  = [(1, []);
                           (2, []);
                           (3, []);
                           (4, [])]);
  "m_n0"  @? (fri m_n0  = []);
  "m_ord" @? (fri m_ord = [(1, [11;21;31;41]);
                           (2, [12;22;32;42]);
                           (3, [13;23;33;43]);
                           (4, [14;24;34;44]);
                           (5, [15;25;35;45])]);
  "m_sub" @? (fri m_sub = [(1, [22; 32]);
                           (2, [23; 33]);
                           (3, [24; 34])])

(* test of Mat.fold_topi *)
let test_fold_topi () =
  let fti a =
    fold_topi (fun i acc rv -> (i, Slap.Vec.to_list rv) :: acc) [] a
  in
  "m_00"  @? (fti m_00  = []);
  "m_0n"  @? (fti m_0n  = []);
  "m_n0"  @? (fti m_n0  = [(4, []);
                           (3, []);
                           (2, []);
                           (1, [])]);
  "m_ord" @? (fti m_ord = [(4, [41;42;43;44;45]);
                           (3, [31;32;33;34;35]);
                           (2, [21;22;23;24;25]);
                           (1, [11;12;13;14;15])]);
  "m_sub" @? (fti m_sub = [(2, [32;33;34]);
                           (1, [22;23;24])])

(* test of Mat.fold_bottomi *)
let test_fold_bottomi () =
  let fbi a =
    fold_bottomi (fun i rv acc -> (i, Slap.Vec.to_list rv) :: acc) a []
  in
  "m_00"  @? (fbi m_00  = []);
  "m_0n"  @? (fbi m_0n  = []);
  "m_n0"  @? (fbi m_n0  = [(1, []);
                           (2, []);
                           (3, []);
                           (4, [])]);
  "m_ord" @? (fbi m_ord = [(1, [11;12;13;14;15]);
                           (2, [21;22;23;24;25]);
                           (3, [31;32;33;34;35]);
                           (4, [41;42;43;44;45])]);
  "m_sub" @? (fbi m_sub = [(1, [22;23;24]);
                           (2, [32;33;34])])

let suite =
  "Slap.Mat" >:::
    ["to_array"     >:: test_to_array;
     "to_list"      >:: test_to_list;
     "of_array_dyn" >:: test_of_array_dyn;
     "of_list_dyn"  >:: test_of_list_dyn;
     "fold_lefti"   >:: test_fold_lefti;
     "fold_righti"  >:: test_fold_righti;
     "fold_topi"    >:: test_fold_topi;
     "fold_bottomi" >:: test_fold_bottomi]
