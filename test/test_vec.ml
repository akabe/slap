(* test_vec.ml *)

open Format
open OUnit
open Slap.Vec
open Bigarray

(* an empty vector *)
let v_emp = init int Slap.Size.zero (fun _ -> 42)
(* a single-element vector *)
let v_sgl = init int Slap.Size.one (fun _ -> 42)
(* an ordinary vector *)
let v_ord = init int Slap.Size.ten (fun i -> i)
(* a contiguous subvector containing the first element in the original vector *)
let sv_cfe = subcntvec_dyn Slap.Size.four ~ofsx:1 v_ord
(* a contiguous subvector containing the last element in the original vector *)
let sv_cle = subcntvec_dyn Slap.Size.four ~ofsx:7 v_ord
(* a discrete subvector containing the first element in the original vector *)
let sv_dfe = subdscvec_dyn Slap.Size.four ~ofsx:1 ~incx:2 v_ord
(* a discrete subvector containing the last element in the original vector *)
let sv_dle = subdscvec_dyn Slap.Size.four ~ofsx:4 ~incx:2 v_ord
(* a discrete subvector with a negetive incrementation *)
let sv_dng = subdscvec_dyn Slap.Size.four ~ofsx:10 ~incx:(-3) v_ord

(* Functions to generate a destination vector *)
let duplicate x = (x, x)
let make0_v_emp () = duplicate (make int Slap.Size.zero 0)
let make0_v_sgl () = duplicate (make int Slap.Size.one 0)
let make0_v_ord () = duplicate (make int Slap.Size.ten 0)

let make0_sv f () =
  let x = make int Slap.Size.ten 0 in
  let y = f x in
  (x, y)

let make0_sv_dle = make0_sv (subdscvec_dyn Slap.Size.four ~ofsx:4 ~incx:2)
let make0_sv_dng = make0_sv (subdscvec_dyn Slap.Size.four ~ofsx:10 ~incx:(-3))

(* test of Slap.Vec.to_array *)
let test_to_array () =
  "v_emp"  @? (to_array v_emp  = [||]);
  "v_sgl"  @? (to_array v_sgl  = [|42|]);
  "v_ord"  @? (to_array v_ord  = [|1;2;3;4;5;6;7;8;9;10|]);
  "sv_cfe" @? (to_array sv_cfe = [|1;2;3;4|]);
  "sv_cle" @? (to_array sv_cle = [|7;8;9;10|]);
  "sv_dfe" @? (to_array sv_dfe = [|1;3;5;7|]);
  "sv_dle" @? (to_array sv_dle = [|4;6;8;10|]);
  "sv_dng" @? (to_array sv_dng = [|10;7;4;1|])

(* test of Slap.Vec.to_list *)
let test_to_list () =
  "v_emp"  @? (to_list v_emp  = []);
  "v_sgl"  @? (to_list v_sgl  = [42]);
  "v_ord"  @? (to_list v_ord  = [1;2;3;4;5;6;7;8;9;10]);
  "sv_cfe" @? (to_list sv_cfe = [1;2;3;4]);
  "sv_cle" @? (to_list sv_cle = [7;8;9;10]);
  "sv_dfe" @? (to_list sv_dfe = [1;3;5;7]);
  "sv_dle" @? (to_list sv_dle = [4;6;8;10]);
  "sv_dng" @? (to_list sv_dng = [10;7;4;1])

(* test of Slap.Vec.of_array_dyn *)
let test_of_array_dyn () =
  let of_array_dyn n a = of_array_dyn int n a in
  let (=) x y = (to_list x = to_list y) in
  "v_emp"  @? (of_array_dyn Slap.Size.zero [||]                     = v_emp);
  "v_sgl"  @? (of_array_dyn Slap.Size.one  [|42|]                   = v_sgl);
  "v_ord"  @? (of_array_dyn Slap.Size.ten  [|1;2;3;4;5;6;7;8;9;10|] = v_ord);
  let (@!) msg f =
    assert_raises ~msg (Invalid_argument "Slap.Vec.of_array_dyn") f
  in
  "exn" @! (fun () -> of_array_dyn Slap.Size.five [|1;2;3;4|])

(* test of Slap.Vec.of_list_dyn *)
let test_of_list_dyn () =
  let of_list_dyn n a = of_list_dyn int n a in
  let (=) x y = (to_list x = to_list y) in
  "v_emp"  @? (of_list_dyn Slap.Size.zero []                     = v_emp);
  "v_sgl"  @? (of_list_dyn Slap.Size.one  [42]                   = v_sgl);
  "v_ord"  @? (of_list_dyn Slap.Size.ten  [1;2;3;4;5;6;7;8;9;10] = v_ord);
  let (@!) msg f =
    assert_raises ~msg (Invalid_argument "Slap.Vec.of_list_dyn") f
  in
  "exn" @! (fun () -> of_list_dyn Slap.Size.five [1;2;3;4])

(* test of Slap.Vec.copy *)
let test_copy () =
  "v_emp"  @? (to_list (copy v_emp)  = []);
  "v_sgl"  @? (to_list (copy v_sgl)  = [42]);
  "v_ord"  @? (to_list (copy v_ord)  = [1;2;3;4;5;6;7;8;9;10]);
  "sv_cfe" @? (to_list (copy sv_cfe) = [1;2;3;4]);
  "sv_cle" @? (to_list (copy sv_cle) = [7;8;9;10]);
  "sv_dfe" @? (to_list (copy sv_dfe) = [1;3;5;7]);
  "sv_dle" @? (to_list (copy sv_dle) = [4;6;8;10]);
  "sv_dng" @? (to_list (copy sv_dng) = [10;7;4;1]);
  let cp mk0 x l =
    let v, sv = mk0 () in
    let y = copy ~y:sv x in
    (to_list x = to_list y) && (to_list x = to_list sv) && (to_list v = l)
  in
  "v_emp <- v_emp"   @? (cp make0_v_emp  v_emp  []);
  "v_sgl <- v_sgl"   @? (cp make0_v_sgl  v_sgl  [42]);
  "v_ord <- v_ord"   @? (cp make0_v_ord  v_ord  [1;2;3;4;5;6;7;8;9;10]);
  "sv_dle <- sv_dfe" @? (cp make0_sv_dle sv_dfe [0;0;0;1;0;3;0;5;0;7]);
  "sv_dle <- sv_dle" @? (cp make0_sv_dle sv_dle [0;0;0;4;0;6;0;8;0;10]);
  "sv_dle <- sv_dng" @? (cp make0_sv_dle sv_dng [0;0;0;10;0;7;0;4;0;1]);
  "sv_dng <- sv_dfe" @? (cp make0_sv_dng sv_dfe [7;0;0;5;0;0;3;0;0;1]);
  "sv_dng <- sv_dle" @? (cp make0_sv_dng sv_dle [10;0;0;8;0;0;6;0;0;4]);
  "sv_dng <- sv_dng" @? (cp make0_sv_dng sv_dng [1;0;0;4;0;0;7;0;0;10])

(* test of Slap.Vec.fold_lefti *)
let test_fold_lefti () =
  let fold_lefti v = fold_lefti (fun i l x -> (i, x) :: l) [] v in
  "v_emp"  @? (fold_lefti v_emp  = []);
  "v_sgl"  @? (fold_lefti v_sgl  = [1,42]);
  "v_ord"  @? (fold_lefti v_ord  = [10,10;9,9;8,8;7,7;6,6;5,5;4,4;3,3;2,2;1,1]);
  "sv_cfe" @? (fold_lefti sv_cfe = [4,4;3,3;2,2;1,1]);
  "sv_cle" @? (fold_lefti sv_cle = [4,10;3,9;2,8;1,7]);
  "sv_dfe" @? (fold_lefti sv_dfe = [4,7;3,5;2,3;1,1]);
  "sv_dle" @? (fold_lefti sv_dle = [4,10;3,8;2,6;1,4]);
  "sv_dng" @? (fold_lefti sv_dng = [4,1;3,4;2,7;1,10])

(* test of Slap.Vec.fold_righti *)
let test_fold_righti () =
  let fold_righti v = fold_righti (fun i x l -> (i, x) :: l) v [] in
  "v_emp"  @? (fold_righti v_emp  = []);
  "v_sgl"  @? (fold_righti v_sgl  = [1,42]);
  "v_ord"  @? (fold_righti v_ord  = [1,1;2,2;3,3;4,4;5,5;6,6;7,7;8,8;9,9;10,10]);
  "sv_cfe" @? (fold_righti sv_cfe = [1,1;2,2;3,3;4,4]);
  "sv_cle" @? (fold_righti sv_cle = [1,7;2,8;3,9;4,10]);
  "sv_dfe" @? (fold_righti sv_dfe = [1,1;2,3;3,5;4,7]);
  "sv_dle" @? (fold_righti sv_dle = [1,4;2,6;3,8;4,10]);
  "sv_dng" @? (fold_righti sv_dng = [1,10;2,7;3,4;4,1])

(* test of Slap.Vec.iteri *)
let test_iteri () =
  let iteri_fl v = (* `fold_lefti' implemented by using `iteri' *)
    let ll = ref [] in
    iteri (fun i x -> ll := (i, x) :: !ll) v;
    !ll
  in
  "v_emp"  @? (iteri_fl v_emp  = []);
  "v_sgl"  @? (iteri_fl v_sgl  = [1,42]);
  "v_ord"  @? (iteri_fl v_ord  = [10,10;9,9;8,8;7,7;6,6;5,5;4,4;3,3;2,2;1,1]);
  "sv_cfe" @? (iteri_fl sv_cfe = [4,4;3,3;2,2;1,1]);
  "sv_cle" @? (iteri_fl sv_cle = [4,10;3,9;2,8;1,7]);
  "sv_dfe" @? (iteri_fl sv_dfe = [4,7;3,5;2,3;1,1]);
  "sv_dle" @? (iteri_fl sv_dle = [4,10;3,8;2,6;1,4]);
  "sv_dng" @? (iteri_fl sv_dng = [4,1;3,4;2,7;1,10])

(* test of Slap.Vec.mapi *)
let test_mapi () =
  let mapi ?y v = mapi int (fun i x -> 2 * x + i) ?y v in
  "v_emp" @? (to_list (mapi v_emp) = []);
  "v_sgl" @? (to_list (mapi v_sgl) = [85]);
  "v_ord" @? (to_list (mapi v_ord) = [3;6;9;12;15;18;21;24;27;30]);
  let mp mk0 x l =
    let org_y, y = mk0 () in
    let y' = mapi ~y x in
    (to_list y = to_list y') && (to_list org_y = l)
  in
  "v_emp <- v_emp"   @? (mp make0_v_emp  v_emp  []);
  "v_sgl <- v_sgl"   @? (mp make0_v_sgl  v_sgl  [85]);
  "v_ord <- v_ord"   @? (mp make0_v_ord  v_ord  [3;6;9;12;15;18;21;24;27;30]);
  "sv_dle <- sv_dfe" @? (mp make0_sv_dle sv_dfe [0;0;0;3;0;8;0;13;0;18]);
  "sv_dle <- sv_dle" @? (mp make0_sv_dle sv_dle [0;0;0;9;0;14;0;19;0;24]);
  "sv_dle <- sv_dng" @? (mp make0_sv_dle sv_dng [0;0;0;21;0;16;0;11;0;6]);
  "sv_dng <- sv_dfe" @? (mp make0_sv_dng sv_dfe [18;0;0;13;0;0;8;0;0;3]);
  "sv_dng <- sv_dle" @? (mp make0_sv_dng sv_dle [24;0;0;19;0;0;14;0;0;9]);
  "sv_dng <- sv_dng" @? (mp make0_sv_dng sv_dng [6;0;0;11;0;0;16;0;0;21])


let suite =
  "Slap.Vec" >:::
    ["to_array"     >:: test_to_array;
     "to_list"      >:: test_to_list;
     "of_array_dyn" >:: test_of_array_dyn;
     "of_list_dyn"  >:: test_of_list_dyn;
     "copy"         >:: test_copy;
     "fold_lefti"   >:: test_fold_lefti;
     "fold_righti"  >:: test_fold_righti;
     "iteri"        >:: test_iteri;
     "mapi"         >:: test_mapi]
