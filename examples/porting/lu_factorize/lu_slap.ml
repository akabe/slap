(** LU factorization *)

open Format
open Slap.D
open Slap.Io
open Slap.Common

let separate_lu m n lu =
  let k = Slap.Size.min m n in
  let u = Mat.init_cols k n (fun i j -> if i > j then 0. else Mat.get_dyn lu i j) in
  let l = Mat.init_cols m k (fun i j ->
    if i < j then 0. else
    if i = j then 1. else Mat.get_dyn lu i j) in
  l, u

let p_of_ipiv m ipiv =
  let k = Slap.Vec.dim ipiv in
  let p = Mat.identity m in
  for i = 1 to Slap.Size.to_int k do
    let j = Int32.to_int (Slap.Vec.get_dyn ipiv i) in
    if i != j then swap (Mat.col_dyn p i) (Mat.col_dyn p j)
  done;
  p

let lu_factorize a =
  let m, n = Mat.dim1 a, Mat.dim2 a in
  let a' = Mat.init_cols m n (fun i j -> Mat.get_dyn a i j) in
  let ipiv = getrf a' in
  let p = p_of_ipiv m ipiv in
  let l, u = separate_lu m n a' in
  p, l, u

let check_result a l u p =
  let a' = gemm ~transa:normal p ~transb:normal
                (gemm ~transa:normal l ~transb:normal u) in
  Mat.axpy ~alpha:(-1.0) ~x:a a';
  let diff_norm = lange ~norm:norm_frob a' in
  diff_norm < 1e-6

let main () =
  let module A = Mat.Of_array(struct
      let value = [|[|-2.; 2.;-3.; 4.|];
                    [|-1.; 1.; 3.; 0.|];
                    [| 2.; 0.;-1.;-3.|]|]
    end) in
  let a = A.value in
  let p, l, u = lu_factorize a in
  printf "A = \n%!%a\n" pp_fmat a;
  printf "L = \n%!%a\n" pp_fmat l;
  printf "U = \n%!%a\n" pp_fmat u;
  printf "P = \n%!%a\n" pp_fmat p;
  printf "\n** %s\n" (if check_result a l u p then "Success!" else "Failure!")

let _ = main ()
