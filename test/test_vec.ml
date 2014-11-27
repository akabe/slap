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

(* test of Slap.Vec.fill *)
let test_fill () =
  let fl mk0 e l =
    let v, sv = mk0 () in
    fill sv e;
    let s1 = List.fold_left (fun s e -> s ^ string_of_int e ^ ";")
                           "" (to_list v) in
    let s2 = List.fold_left (fun s e -> s ^ string_of_int e ^ ";")
                           "" (to_list sv) in
    List.for_all ((=) e) (to_list sv) && (to_list v = l)
  in
  "v_emp"  @? (fl make0_v_emp  42 []);
  "v_sgl"  @? (fl make0_v_sgl  42 [42]);
  "v_ord"  @? (fl make0_v_ord  42 [42;42;42;42;42;42;42;42;42;42]);
  "sv_dle" @? (fl make0_sv_dle 42 [ 0; 0; 0;42; 0;42; 0;42; 0;42]);
  "sv_dng" @? (fl make0_sv_dng 42 [42; 0; 0;42; 0; 0;42; 0; 0;42])

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

(* test of Slap.Vec.fold_lefti2 *)
let test_fold_lefti2 () =
  let fold_lefti2 = fold_lefti2 (fun i l x y -> (i, x, y) :: l) [] in
  "sv_dfe & sv_dle" @?(fold_lefti2 sv_dfe sv_dle = [4,7,10;3,5,8;2,3,6;1,1,4]);
  "sv_dfe & sv_dng" @?(fold_lefti2 sv_dfe sv_dng = [4,7,1;3,5,4;2,3,7;1,1,10]);
  "sv_dle & sv_dfe" @?(fold_lefti2 sv_dle sv_dfe = [4,10,7;3,8,5;2,6,3;1,4,1]);
  "sv_dle & sv_dng" @?(fold_lefti2 sv_dle sv_dng = [4,10,1;3,8,4;2,6,7;1,4,10]);
  "sv_dng & sv_dfe" @?(fold_lefti2 sv_dng sv_dfe = [4,1,7;3,4,5;2,7,3;1,10,1]);
  "sv_dng & sv_dle" @?(fold_lefti2 sv_dng sv_dle = [4,1,10;3,4,8;2,7,6;1,10,4])

(* test of Slap.Vec.fold_righti2 *)
let test_fold_righti2 () =
  let fold_righti2 x y = fold_righti2 (fun i x y l -> (i, x, y) :: l) x y [] in
  "sv_dfe & sv_dle"@?(fold_righti2 sv_dfe sv_dle = [1,1,4;2,3,6;3,5,8;4,7,10]);
  "sv_dfe & sv_dng"@?(fold_righti2 sv_dfe sv_dng = [1,1,10;2,3,7;3,5,4;4,7,1]);
  "sv_dle & sv_dfe"@?(fold_righti2 sv_dle sv_dfe = [1,4,1;2,6,3;3,8,5;4,10,7]);
  "sv_dle & sv_dng"@?(fold_righti2 sv_dle sv_dng = [1,4,10;2,6,7;3,8,4;4,10,1]);
  "sv_dng & sv_dfe"@?(fold_righti2 sv_dng sv_dfe = [1,10,1;2,7,3;3,4,5;4,1,7]);
  "sv_dng & sv_dle"@?(fold_righti2 sv_dng sv_dle = [1,10,4;2,7,6;3,4,8;4,1,10])

(* test of Slap.Vec.iteri2 *)
let test_iteri2 () =
  let iteri2_fl2 x y = (* `fold_lefti2' implemented by using `iteri2' *)
    let ll = ref [] in
    iteri2 (fun i x y -> ll := (i, x, y) :: !ll) x y;
    !ll
  in
  "sv_dfe & sv_dle" @? (iteri2_fl2 sv_dfe sv_dle = [4,7,10;3,5,8;2,3,6;1,1,4]);
  "sv_dfe & sv_dng" @? (iteri2_fl2 sv_dfe sv_dng = [4,7,1;3,5,4;2,3,7;1,1,10]);
  "sv_dle & sv_dfe" @? (iteri2_fl2 sv_dle sv_dfe = [4,10,7;3,8,5;2,6,3;1,4,1]);
  "sv_dle & sv_dng" @? (iteri2_fl2 sv_dle sv_dng = [4,10,1;3,8,4;2,6,7;1,4,10]);
  "sv_dng & sv_dfe" @? (iteri2_fl2 sv_dng sv_dfe = [4,1,7;3,4,5;2,7,3;1,10,1]);
  "sv_dng & sv_dle" @? (iteri2_fl2 sv_dng sv_dle = [4,1,10;3,4,8;2,7,6;1,10,4])

(* test of Slap.Vec.mapi2 *)
let test_mapi2 () =
  let mp2 mk0 x y l =
    let org_z, z = mk0 () in
    let z' = mapi2 int (fun i x y -> x - y + i) ~z x y in
    (to_list z = to_list z') && (to_list org_z = l)
  in
  "sv_dle <- sv_dfe & sv_dle"
  @? (mp2 make0_sv_dle sv_dfe sv_dle [0;0;0;-2;0;-1;0;0;0;1]);
  "sv_dle <- sv_dfe & sv_dng"
  @? (mp2 make0_sv_dle sv_dfe sv_dng [0;0;0;-8;0;-2;0;4;0;10]);
  "sv_dle <- sv_dle & sv_dfe"
  @? (mp2 make0_sv_dle sv_dle sv_dfe [0;0;0;4;0;5;0;6;0;7]);
  "sv_dle <- sv_dle & sv_dng"
  @? (mp2 make0_sv_dle sv_dle sv_dng [0;0;0;-5;0;1;0;7;0;13]);
  "sv_dle <- sv_dng & sv_dfe"
  @? (mp2 make0_sv_dle sv_dng sv_dfe [0;0;0;10;0;6;0;2;0;-2]);
  "sv_dle <- sv_dng & sv_dle"
  @? (mp2 make0_sv_dle sv_dng sv_dle [0;0;0;7;0;3;0;-1;0;-5]);
  "sv_dng <- sv_dfe & sv_dle"
  @? (mp2 make0_sv_dng sv_dfe sv_dle [1;0;0;0;0;0;-1;0;0;-2])

(* test of Slap.Vec.fold_lefti3 *)
let test_fold_lefti3 () =
  let fli3 = fold_lefti3 (fun i l x y z -> (i, x, y, z) :: l) [] in
  "sv_dfe & sv_dle & sv_dng"
  @? (fli3 sv_dfe sv_dle sv_dng = [4,7,10,1;3,5,8,4;2,3,6,7;1,1,4,10]);
  "sv_dle & sv_dng & sv_dfe"
  @? (fli3 sv_dle sv_dng sv_dfe = [4,10,1,7;3,8,4,5;2,6,7,3;1,4,10,1]);
  "sv_dng & sv_dfe & sv_dle"
  @? (fli3 sv_dng sv_dfe sv_dle = [4,1,7,10;3,4,5,8;2,7,3,6;1,10,1,4])

(* test of Slap.Vec.fold_righti3 *)
let test_fold_righti3 () =
  let fri3 x y z = fold_righti3 (fun i x y z l -> (i, x, y, z) :: l) x y z [] in
  "sv_dfe & sv_dle & sv_dng"
  @? (fri3 sv_dfe sv_dle sv_dng = [1,1,4,10;2,3,6,7;3,5,8,4;4,7,10,1]);
  "sv_dle & sv_dng & sv_dfe"
  @? (fri3 sv_dle sv_dng sv_dfe = [1,4,10,1;2,6,7,3;3,8,4,5;4,10,1,7]);
  "sv_dng & sv_dfe & sv_dle"
  @? (fri3 sv_dng sv_dfe sv_dle = [1,10,1,4;2,7,3,6;3,4,5,8;4,1,7,10])

(* test of Slap.Vec.iteri3 *)
let test_iteri3 () =
  let iteri3_fl3 x y z = (* `fold_lefti3' implemented by using `iteri3' *)
    let ll = ref [] in
    iteri3 (fun i x y z -> ll := (i, x, y, z) :: !ll) x y z;
    !ll
  in
  "sv_dfe & sv_dle & sv_dng"
  @? (iteri3_fl3 sv_dfe sv_dle sv_dng = [4,7,10,1;3,5,8,4;2,3,6,7;1,1,4,10]);
  "sv_dle & sv_dng & sv_dfe"
  @? (iteri3_fl3 sv_dle sv_dng sv_dfe = [4,10,1,7;3,8,4,5;2,6,7,3;1,4,10,1]);
  "sv_dng & sv_dfe & sv_dle"
  @? (iteri3_fl3 sv_dng sv_dfe sv_dle = [4,1,7,10;3,4,5,8;2,7,3,6;1,10,1,4])

(* test of Slap.Vec.mapi3 *)
let test_mapi3 () =
  let mp3 mk0 x y z l =
    let org_w, w = mk0 () in
    let w' = mapi3 int (fun i x y z -> (x - y) * z + i) ~w x y z in
    (to_list w = to_list w') && (to_list org_w = l)
  in
  "sv_dle <- sv_dfe & sv_dle & sv_dng"
  @? (mp3 make0_sv_dle sv_dfe sv_dle sv_dng [0;0;0;-29;0;-19;0;-9;0;1]);
  "sv_dle <- sv_dle & sv_dng & sv_dfe"
  @? (mp3 make0_sv_dle sv_dle sv_dng sv_dfe [0;0;0;-5;0;-1;0;23;0;67]);
  "sv_dle <- sv_dng & sv_dfe & sv_dle"
  @? (mp3 make0_sv_dle sv_dng sv_dfe sv_dle [0;0;0;37;0;26;0;-5;0;-56]);
  "sv_dng <- sv_dfe & sv_dle & sv_dng"
  @? (mp3 make0_sv_dng sv_dfe sv_dle sv_dng [1;0;0;-9;0;0;-19;0;0;-29]);
  "sv_dng <- sv_dle & sv_dng & sv_dfe"
  @? (mp3 make0_sv_dng sv_dle sv_dng sv_dfe [67;0;0;23;0;0;-1;0;0;-5]);
  "sv_dng <- sv_dng & sv_dfe & sv_dle"
  @? (mp3 make0_sv_dng sv_dng sv_dfe sv_dle [-56;0;0;-5;0;0;26;0;0;37])

(* test of Slap.Vec.for_all *)
let test_for_all () =
  "v_emp"    @? (for_all (fun _ -> false) v_emp  = true);
  "v_ord/F"  @? (for_all (fun x -> x > 1) v_ord  = false);
  "v_ord/T"  @? (for_all (fun x -> x > 0) v_ord  = true);
  "sv_dfe/F" @? (for_all (fun x -> x < 7) sv_dfe = false);
  "sv_dfe/T" @? (for_all (fun x -> x < 8) sv_dfe = true);
  "sv_dle/F" @? (for_all (fun x -> x > 4) sv_dle = false);
  "sv_dle/T" @? (for_all (fun x -> x > 3) sv_dle = true);
  "sv_dng/F" @? (for_all (fun x -> x > 1) sv_dng = false);
  "sv_dng/T" @? (for_all (fun x -> x > 0) sv_dng = true)

(* test of Slap.Vec.exists *)
let test_exists () =
  "v_emp"    @? (exists (fun _ -> true)  v_emp  = false);
  "v_ord/F"  @? (exists (fun x -> x = 0) v_ord  = false);
  "v_ord/T"  @? (exists (fun x -> x = 3) v_ord  = true);
  "sv_dfe/F" @? (exists (fun x -> x = 2) sv_dfe = false);
  "sv_dfe/T" @? (exists (fun x -> x = 3) sv_dfe = true);
  "sv_dle/F" @? (exists (fun x -> x = 7) sv_dle = false);
  "sv_dle/T" @? (exists (fun x -> x = 8) sv_dle = true);
  "sv_dng/F" @? (exists (fun x -> x = 2) sv_dng = false);
  "sv_dng/T" @? (exists (fun x -> x = 1) sv_dng = true)

(* test of Slap.Vec.for_all2 *)
let test_for_all2 () =
  "sv_dfe & sv_dle" @? (for_all2 (fun x y -> x + y >  5) sv_dfe sv_dle = false);
  "sv_dle & sv_dfe" @? (for_all2 (fun x y -> x + y >  4) sv_dle sv_dfe = true);
  "sv_dfe & sv_dng" @? (for_all2 (fun x y -> x + y >  8) sv_dfe sv_dng = false);
  "sv_dng & sv_dfe" @? (for_all2 (fun x y -> x + y >  7) sv_dng sv_dfe = true);
  "sv_dle & sv_dng" @? (for_all2 (fun x y -> x + y > 11) sv_dle sv_dng = false);
  "sv_dng & sv_dle" @? (for_all2 (fun x y -> x + y > 10) sv_dng sv_dle = true)

(* test of Slap.Vec.exists2 *)
let test_exists2 () =
  "sv_dfe & sv_dle" @? (exists2 (fun x y -> x + y =  6) sv_dfe sv_dle = false);
  "sv_dle & sv_dfe" @? (exists2 (fun x y -> x + y =  5) sv_dle sv_dfe = true);
  "sv_dfe & sv_dng" @? (exists2 (fun x y -> x + y =  7) sv_dfe sv_dng = false);
  "sv_dng & sv_dfe" @? (exists2 (fun x y -> x + y =  8) sv_dng sv_dfe = true);
  "sv_dle & sv_dng" @? (exists2 (fun x y -> x + y = 10) sv_dle sv_dng = false);
  "sv_dng & sv_dle" @? (exists2 (fun x y -> x + y = 11) sv_dng sv_dle = true)


let suite =
  "Slap.Vec" >:::
    ["to_array"     >:: test_to_array;
     "to_list"      >:: test_to_list;
     "of_array_dyn" >:: test_of_array_dyn;
     "of_list_dyn"  >:: test_of_list_dyn;
     "copy"         >:: test_copy;
     "fill"         >:: test_fill;
     "fold_lefti"   >:: test_fold_lefti;
     "fold_righti"  >:: test_fold_righti;
     "iteri"        >:: test_iteri;
     "mapi"         >:: test_mapi;
     "fold_lefti2"  >:: test_fold_lefti2;
     "fold_righti2" >:: test_fold_righti2;
     "iteri2"       >:: test_iteri2;
     "mapi2"        >:: test_mapi2;
     "fold_lefti3"  >:: test_fold_lefti3;
     "fold_righti3" >:: test_fold_righti3;
     "iteri3"       >:: test_iteri3;
     "mapi3"        >:: test_mapi3;
     "for_all"      >:: test_for_all;
     "exists"       >:: test_exists;
     "for_all2"     >:: test_for_all2;
     "exists2"      >:: test_exists2]
