(* test_mat.ml *)

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

(* test of Slap.Mat.to_bigarray *)
let test_to_bigarray () =
  let create m n = Array2.create int fortran_layout m n in
  let of_array a = Array2.of_array int fortran_layout a in
  "m_00"  @? (to_bigarray m_00  = create 0 0);
  "m_0n"  @? (to_bigarray m_0n  = create 0 4);
  "m_n0"  @? (to_bigarray m_n0  = of_array [|[||];[||];[||];[||]|]);
  "m_ord" @? (to_bigarray m_ord = of_array [|[|11;12;13;14;15|];
                                             [|21;22;23;24;25|];
                                             [|31;32;33;34;35|];
                                             [|41;42;43;44;45|]|]);
  "m_sub" @? (to_bigarray m_sub = of_array [|[|22;23;24|];
                                             [|32;33;34|]|])

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

(* test of Slap.Mat.of_bigarray_dyn *)
let test_of_bigarray_dyn () =
  let of_bigarray_dyn m n a =
    let ba = Array2.of_array int fortran_layout a in
    of_bigarray_dyn m n ba
  in
  let (=) x y = (to_list x = to_list y) in
  "m_00"  @? (of_bigarray_dyn zero zero [||] = m_00);
  "m_0n"  @? (of_bigarray_dyn zero four [||] = m_0n);
  "m_n0"  @? (of_bigarray_dyn four zero [|[||];[||];[||];[||]|] = m_n0);
  "m_ord" @? (of_bigarray_dyn four five [|[|11;12;13;14;15|];
                                          [|21;22;23;24;25|];
                                          [|31;32;33;34;35|];
                                          [|41;42;43;44;45|]|] = m_ord);
  let (@!) msg f =
    assert_raises ~msg (Invalid_argument "Slap.Mat.of_bigarray_dyn") f
  in
  "exn/rows" @! (fun () -> of_bigarray_dyn two three [|[|11;12|];
                                                       [|21;22|]|]);
  "exn/cols" @! (fun () -> of_bigarray_dyn two three [|[|11;12;13|]|])

(* test of packed *)
let test_packed () =
  let m_sq_00  = m_00 in
  let m_sq_ord = init int five five (fun i j -> 10 * i + j) in
  let m_sq_sub = submat_dyn three three ~ar:2 ~ac:2 m_sq_ord in
  let (=) x y = (Slap.Vec.to_list x = y) in
  "m_sq_00, up=true"  @? (packed ~up:true  m_sq_00  = []);
  "m_sq_ord,up=true"  @? (packed ~up:true  m_sq_ord = [11;
                                                       12;22;
                                                       13;23;33;
                                                       14;24;34;44;
                                                       15;25;35;45;55]);
  "m_sq_sub,up=true"  @? (packed ~up:true  m_sq_sub = [22;
                                                       23;33;
                                                       24;34;44]);
  "m_sq_00, up=false" @? (packed ~up:false m_sq_00  = []);
  "m_sq_ord,up=false" @? (packed ~up:false m_sq_ord = [11;21;31;41;51;
                                                       22;32;42;52;
                                                       33;43;53;
                                                       44;54;
                                                       55]);
  "m_sq_sub,up=false" @? (packed ~up:false m_sq_sub = [22;32;42;
                                                       33;43;
                                                       44])

(* test of unpacked *)
let test_unpacked () =
  let pv_zero = Slap.Vec.init int (Slap.Size.packed zero) (fun i -> i) in
  let pv_ord  = Slap.Vec.init int (Slap.Size.packed five) (fun i -> i) in
  let pv_sub  = Slap.Vec.subcntvec_dyn (Slap.Size.packed three) ~ofsx:3 pv_ord in
  let unpacked up v = unpacked ~up ~fill_num:(Some 0) v in
  let (=) x y = (to_list x = y) in
  "pv_zero,up=true"  @? (unpacked true  pv_zero = []);
  "pv_ord, up=true"  @? (unpacked true  pv_ord  = [[1;2;4; 7;11];
                                                   [0;3;5; 8;12];
                                                   [0;0;6; 9;13];
                                                   [0;0;0;10;14];
                                                   [0;0;0; 0;15]]);
  "pv_sub, up=true"  @? (unpacked true  pv_sub  = [[3;4;6];
                                                   [0;5;7];
                                                   [0;0;8]]);
  "pv_ord, up=false" @? (unpacked false pv_ord  = [[1;0; 0; 0; 0];
                                                   [2;6; 0; 0; 0];
                                                   [3;7;10; 0; 0];
                                                   [4;8;11;13; 0];
                                                   [5;9;12;14;15]]);
  "pv_sub, up=false" @? (unpacked false pv_sub  = [[3;0;0];
                                                   [4;6;0];
                                                   [5;7;8]])

(* test of geband_dyn *)
let test_geband_dyn () =
  let module M = Of_int_dyn(struct let value = 7 end) in
  let module N = Of_int_dyn(struct let value = 9 end) in
  let six = succ five in
  let eight = succ (succ six) in
  let a = init int M.value N.value (fun i j -> 10 * i + j) in
  let geband_dyn kl ku =
    let b = make int (Slap.Size.geband_dyn (dim1 a) (dim2 a) kl ku)
                 (dim2 a) 0 in
    to_list (geband_dyn kl ku ~b a)
  in
  "ordinary" @? (geband_dyn two one = [[ 0;12;23;34;45;56;67;78; 0];
                                       [11;22;33;44;55;66;77; 0; 0];
                                       [21;32;43;54;65;76; 0; 0; 0];
                                       [31;42;53;64;75; 0; 0; 0; 0]]);
  "upper" @? (geband_dyn zero eight = [[ 0; 0; 0; 0; 0; 0; 0; 0;19];
                                       [ 0; 0; 0; 0; 0; 0; 0;18;29];
                                       [ 0; 0; 0; 0; 0; 0;17;28;39];
                                       [ 0; 0; 0; 0; 0;16;27;38;49];
                                       [ 0; 0; 0; 0;15;26;37;48;59];
                                       [ 0; 0; 0;14;25;36;47;58;69];
                                       [ 0; 0;13;24;35;46;57;68;79];
                                       [ 0;12;23;34;45;56;67;78; 0];
                                       [11;22;33;44;55;66;77; 0; 0]]);
  "lower" @? (geband_dyn six zero = [[11;22;33;44;55;66;77; 0; 0];
                                     [21;32;43;54;65;76; 0; 0; 0];
                                     [31;42;53;64;75; 0; 0; 0; 0];
                                     [41;52;63;74; 0; 0; 0; 0; 0];
                                     [51;62;73; 0; 0; 0; 0; 0; 0];
                                     [61;72; 0; 0; 0; 0; 0; 0; 0];
                                     [71; 0; 0; 0; 0; 0; 0; 0; 0]]);
  "diagonal" @? (geband_dyn zero zero = [[11;22;33;44;55;66;77; 0; 0]])

(* test of ungeband *)
let test_ungeband () =
  let module M = Of_int_dyn(struct let value = 7 end) in
  let module N = Of_int_dyn(struct let value = 9 end) in
  let six = succ five in
  let eight = succ (succ six) in
  let a = init int M.value N.value (fun i j -> 10 * i + j) in
  let band kl ku =
    let b = geband_dyn kl ku a in
    let a' = ungeband (dim1 a) kl ku ~fill_num:(Some 0) b in
    to_list a'
  in
  "ordinary" @? (band two one = [[11;12; 0; 0; 0; 0; 0; 0; 0];
                                 [21;22;23; 0; 0; 0; 0; 0; 0];
                                 [31;32;33;34; 0; 0; 0; 0; 0];
                                 [ 0;42;43;44;45; 0; 0; 0; 0];
                                 [ 0; 0;53;54;55;56; 0; 0; 0];
                                 [ 0; 0; 0;64;65;66;67; 0; 0];
                                 [ 0; 0; 0; 0;75;76;77;78; 0]]);
  "upper" @? (band zero eight = [[11;12;13;14;15;16;17;18;19];
                                 [ 0;22;23;24;25;26;27;28;29];
                                 [ 0; 0;33;34;35;36;37;38;39];
                                 [ 0; 0; 0;44;45;46;47;48;49];
                                 [ 0; 0; 0; 0;55;56;57;58;59];
                                 [ 0; 0; 0; 0; 0;66;67;68;69];
                                 [ 0; 0; 0; 0; 0; 0;77;78;79]]);
  "lower" @? (band six zero = [[11; 0; 0; 0; 0; 0; 0; 0; 0];
                               [21;22; 0; 0; 0; 0; 0; 0; 0];
                               [31;32;33; 0; 0; 0; 0; 0; 0];
                               [41;42;43;44; 0; 0; 0; 0; 0];
                               [51;52;53;54;55; 0; 0; 0; 0];
                               [61;62;63;64;65;66; 0; 0; 0];
                               [71;72;73;74;75;76;77; 0; 0]]);
  "diagonal" @? (band zero zero = [[11; 0; 0; 0; 0; 0; 0; 0; 0];
                                   [ 0;22; 0; 0; 0; 0; 0; 0; 0];
                                   [ 0; 0;33; 0; 0; 0; 0; 0; 0];
                                   [ 0; 0; 0;44; 0; 0; 0; 0; 0];
                                   [ 0; 0; 0; 0;55; 0; 0; 0; 0];
                                   [ 0; 0; 0; 0; 0;66; 0; 0; 0];
                                   [ 0; 0; 0; 0; 0; 0;77; 0; 0]])

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
     "to_bigarray"  >:: test_to_bigarray;
     "of_array_dyn" >:: test_of_array_dyn;
     "of_list_dyn"  >:: test_of_list_dyn;
     "of_bigarray_dyn" >:: test_of_bigarray_dyn;
     "packed"       >:: test_packed;
     "unpacked"     >:: test_unpacked;
     "geband_dyn"   >:: test_geband_dyn;
     "ungeband"     >:: test_ungeband;
     "fold_lefti"   >:: test_fold_lefti;
     "fold_righti"  >:: test_fold_righti;
     "fold_topi"    >:: test_fold_topi;
     "fold_bottomi" >:: test_fold_bottomi]
