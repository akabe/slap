(* test_array.ml *)

open OUnit
open Slap.Size
open Slap.Array

let sa_emp1 = make zero ""
let sa_sgl1 = make one "foo"
let sa_ord1 = init five (fun i -> [|"zero";"one";"two";"three";"four"|].(i))
let sa_emp2 = make zero 42
let sa_sgl2 = make one 42
let sa_ord2 = init five (fun i -> [|12;34;56;78;90|].(i))

(* test Slap.Array.init_matrix *)
let test_init_matrix () =
  let init_matrix m n = init_matrix m n (fun i j -> 10 * i + j) in
  let (=) sa a = (Array.map to_array (to_array sa) = a) in
  "0x0" @? (init_matrix zero zero = [||]);
  "0x5" @? (init_matrix zero five = [||]);
  "5x0" @? (init_matrix five zero = [|[||];[||];[||];[||];[||]|]);
  "3x2" @? (init_matrix three two = [|[|00;01|];[|10;11|];[|20;21|]|]);
  "2x3" @? (init_matrix two three = [|[|00;01;02|];[|10;11;12|]|]);
  "4x4" @? (init_matrix four four = [|[|00;01;02;03|];
                                      [|10;11;12;13|];
                                      [|20;21;22;23|];
                                      [|30;31;32;33|]|])

(* test Slap.Array.fold_lefti *)
let test_fold_lefti () =
  let f i acc x = (i, x) :: acc in
  "sa_emp" @? (fold_lefti f [] sa_emp1 = []);
  "sa_sgl" @? (fold_lefti f [] sa_sgl1 = [0,"foo"]);
  "sa_ord" @? (fold_lefti f [] sa_ord1 = [4,"four";3,"three";2,"two";
                                          1,"one";0,"zero"])

(* test Slap.Array.fold_righti *)
let test_fold_righti () =
  let f i x acc = (i, x) :: acc in
  "sa_emp" @? (fold_righti f sa_emp1 [] = []);
  "sa_sgl" @? (fold_righti f sa_sgl1 [] = [0,"foo"]);
  "sa_ord" @? (fold_righti f sa_ord1 [] = [0,"zero";1,"one";2,"two";
                                           3,"three";4,"four"])

(* test of Slap.Array.mapi2 *)
let test_mapi2 () =
  let f i x y = (i, x ^ string_of_int y) in
  let (=) sa a = (to_array sa = a) in
  "sa_emp" @? (mapi2 f sa_emp1 sa_emp2 = [||]);
  "sa_sgl" @? (mapi2 f sa_sgl1 sa_sgl2 = [|0,"foo42"|]);
  "sa_ord" @? (mapi2 f sa_ord1 sa_ord2 = [|0,"zero12";1,"one34";2,"two56";
                                           3,"three78";4,"four90"|])

(* test Slap.Array.fold_lefti2 *)
let test_fold_lefti2 () =
  let f i acc x y = (i, x ^ string_of_int y) :: acc in
  "sa_emp" @? (fold_lefti2 f [] sa_emp1 sa_emp2 = []);
  "sa_sgl" @? (fold_lefti2 f [] sa_sgl1 sa_sgl2 = [0,"foo42"]);
  "sa_ord" @? (fold_lefti2 f [] sa_ord1 sa_ord2 =
               [4,"four90";3,"three78";2,"two56";1,"one34";0,"zero12"])

(* test Slap.Array.fold_righti2 *)
let test_fold_righti2 () =
  let f i x y acc = (i, x ^ string_of_int y) :: acc in
  "sa_emp" @? (fold_righti2 f sa_emp1 sa_emp2 [] = []);
  "sa_sgl" @? (fold_righti2 f sa_sgl1 sa_sgl2 [] = [0,"foo42"]);
  "sa_ord" @? (fold_righti2 f sa_ord1 sa_ord2 [] =
               [0,"zero12";1,"one34";2,"two56";3,"three78";4,"four90"])

(* test of Slap.Array.mapi3 *)
let test_mapi3 () =
  let f i x y z = (i, x ^ string_of_int y ^ z) in
  let (=) sa a = (to_array sa = a) in
  "sa_emp" @? (mapi3 f sa_emp1 sa_emp2 sa_emp1 = [||]);
  "sa_sgl" @? (mapi3 f sa_sgl1 sa_sgl2 sa_sgl1 = [|0,"foo42foo"|]);
  "sa_ord" @? (mapi3 f sa_ord1 sa_ord2 sa_ord1 =
               [|0,"zero12zero";1,"one34one";2,"two56two";
                 3,"three78three";4,"four90four"|])

(* test Slap.Array.fold_lefti3 *)
let test_fold_lefti3 () =
  let f i acc x y z = (i, x ^ string_of_int y ^ z) :: acc in
  "sa_emp" @? (fold_lefti3 f [] sa_emp1 sa_emp2 sa_emp1 = []);
  "sa_sgl" @? (fold_lefti3 f [] sa_sgl1 sa_sgl2 sa_sgl1 = [0,"foo42foo"]);
  "sa_ord" @? (fold_lefti3 f [] sa_ord1 sa_ord2 sa_ord1 =
               [4,"four90four";3,"three78three";2,"two56two";
                1,"one34one";0,"zero12zero"])

(* test Slap.Array.fold_righti3 *)
let test_fold_righti3 () =
  let f i x y z acc = (i, x ^ string_of_int y ^ z) :: acc in
  "sa_emp" @? (fold_righti3 f sa_emp1 sa_emp2 sa_emp1 [] = []);
  "sa_sgl" @? (fold_righti3 f sa_sgl1 sa_sgl2 sa_sgl1 [] = [0,"foo42foo"]);
  "sa_ord" @? (fold_righti3 f sa_ord1 sa_ord2 sa_ord1 [] =
               [0,"zero12zero";1,"one34one";2,"two56two";
                3,"three78three";4,"four90four"])

(* test Slap.Array.for_all *)
let test_for_all () =
  "sa_emp"   @? (for_all (fun _ -> false) sa_emp2 = true);
  "sa_sgl#1" @? (for_all (fun x -> x = 42) sa_sgl2 = true);
  "sa_sgl#2" @? (for_all (fun x -> x <> 42) sa_sgl2 = false);
  "sa_ord#1" @? (for_all (fun _ -> false) sa_ord2 = false);
  "sa_ord#2" @? (for_all (fun x -> x < 90) sa_ord2 = false);
  "sa_ord#3" @? (for_all (fun x -> x < 100) sa_ord2 = true)

(* test Slap.Array.exists *)
let test_exists () =
  "sa_emp"   @? (exists (fun _ -> true) sa_emp2 = false);
  "sa_sgl#1" @? (exists (fun x -> x = 42) sa_sgl2 = true);
  "sa_sgl#2" @? (exists (fun x -> x <> 42) sa_sgl2 = false);
  "sa_ord#1" @? (exists (fun _ -> false) sa_ord2 = false);
  "sa_ord#2" @? (exists (fun x -> x < 20) sa_ord2 = true);
  "sa_ord#3" @? (exists (fun x -> x < 100) sa_ord2 = true)

let suite =
  "Slap.Array" >:::
  ["init_matrix" >:: test_init_matrix;
   "fold_lefti" >:: test_fold_lefti;
   "fold_righti" >:: test_fold_righti;
   "mapi2" >:: test_mapi2;
   "fold_lefti2" >:: test_fold_lefti2;
   "fold_righti2" >:: test_fold_righti2;
   "mapi3" >:: test_mapi3;
   "fold_lefti3" >:: test_fold_lefti3;
   "fold_righti3" >:: test_fold_righti3;
   "for_all" >:: test_for_all;
   "exists" >:: test_exists]
