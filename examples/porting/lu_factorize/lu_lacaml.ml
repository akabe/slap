(** LU factorization *)

open Format
open Lacaml.D
open Lacaml.Io

let separate_lu m n lu =
  let k = min m n in
  let u = Mat.init_cols k n (fun i j -> if i > j then 0. else lu.{i,j}) in
  let l = Mat.init_cols m k (fun i j ->
    if i < j then 0. else
    if i = j then 1. else lu.{i,j}) in
  l, u

let p_of_ipiv m ipiv =
  let k = Bigarray.Array1.dim ipiv in
  let p = Mat.identity m in
  for i = 1 to k do
    let j = Int32.to_int ipiv.{i} in
    if i != j then swap ~x:(Mat.col p i) (Mat.col p j)
  done;
  p

let lu_factorize a =
  let m, n = Mat.dim1 a, Mat.dim2 a in
  let a' = Mat.init_cols m n (fun i j -> a.{i,j}) in
  let ipiv = getrf a' in
  let p = p_of_ipiv m ipiv in
  let l, u = separate_lu m n a' in
  p, l, u

let check_result a l u p =
  let a' = gemm p (gemm l u) in
  Mat.axpy ~alpha:(-1.0) a a';
  let diff_norm = lange ~norm:`F a' in
  diff_norm < 1e-6

let main () =
  let a = Mat.of_array
    [|[|-2.; 2.;-3.; 4.|];
      [|-1.; 1.; 3.; 0.|];
      [| 2.; 0.;-1.;-3.|]|] in
  let p, l, u = lu_factorize a in
  printf "A = \n%!%a\n" pp_fmat a;
  printf "L = \n%!%a\n" pp_fmat l;
  printf "U = \n%!%a\n" pp_fmat u;
  printf "P = \n%!%a\n" pp_fmat p;
  printf "\n** %s\n" (if check_result a l u p then "Success!" else "Failure!")

let _ = main ()
