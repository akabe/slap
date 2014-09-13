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


let suite =
  "Slap.Mat" >:::
    ["to_array"     >:: test_to_array;
     "to_list"      >:: test_to_list;
     "of_array_dyn" >:: test_of_array_dyn;
     "of_list_dyn"  >:: test_of_list_dyn]
