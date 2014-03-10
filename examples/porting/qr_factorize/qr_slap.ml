(** QR factorization *)

open Format
open Slap.D
open Slap.Io
open Slap.Common

let qr_factorize a =
  let m = Mat.dim1 a in
  let n = Mat.dim2 a in
  let a' = Mat.init_cols m n (fun i j -> Mat.get_dyn a i j) in
  let tau = geqrf a' in
  let r = Mat.init_cols m n (fun i j -> if i > j then 0. else Mat.get_dyn a' i j) in
  orgqr_dyn ~tau a';
  let q = Mat.init_cols m m (fun i j -> Mat.get_dyn a' i j) in
  q, r

let check_result a q r =
  let a' = gemm ~transa:normal q ~transb:normal r in
  Mat.axpy ~alpha:(-1.0) ~x:a a';
  let diff_norm = lange ~norm:norm_frob a' in
  diff_norm < 1e-6

let main () =
  let module A = Mat.Of_array_dyn(struct
      let value = [|[|-3.; 1.;-4.|];
                    [| 1.; 5.; 9.|];
                    [| 2.;-6.; 5.|]|]
    end) in
  let a = A.value in
  let q, r = qr_factorize a in
  printf "A = \n%!%a\n" pp_fmat a;
  printf "R = \n%!%a\n" pp_fmat r;
  printf "Q = \n%!%a\n" pp_fmat q;
  printf "\n** %s\n" (if check_result a q r then "Success!" else "Failure!")

let _ = main ()
