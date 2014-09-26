(* gesv_gbsv.ml -- Solve linear equations of a general (band) matrix *)

open Format
open Slap.Io
open Slap.D
open Slap.Common

let () =
  (* a 4-by-4 band matrix with two subdiagonals and one superdiangonal *)
  let a = Mat.of_list_dyn Slap.Size.four Slap.Size.four
                          [[ 1.; 4.; 0.; 0.];
                           [ 5.;-1.; 3.; 0.];
                           [-4.; 1.;-2.;-1.];
                           [ 0.;-3.;-1.; 0.]] in
  let b = Mat.random Slap.Size.four Slap.Size.two in

  printf "Solve a * x = b where@.";
  printf "  a = @[[%a]@]@." pp_fmat a;
  printf "  b = @[[%a]@]@.@." pp_fmat b;

  (* gesv solves `A*X = B' with matrices A, B and X. *)
  let a' = lacpy a in
  let x = lacpy b in
  gesv a' x;
  printf "gesv: x = @[[%a]@]@." pp_fmat x;
  printf "      a*x = @[[%a]@]@." pp_fmat (gemm ~transa:normal a
                                                ~transb:normal x);

  (* gbsv solves `A*X = B' with matrices B and X, and band matrix A. *)
  let kl = Slap.Size.two in
  let ku = Slap.Size.one in
  let ab = Mat.luband_dyn kl ku a in
  let x = lacpy b in
  gbsv ab kl ku x;
  printf "gbsv: x = @[[%a]@]@." pp_fmat x;
  printf "      a*x = @[[%a]@]@." pp_fmat (gemm ~transa:normal a
                                                ~transb:normal x)
