(* posv_ppsv_pbsv.ml -- Solve linear equations of a symmetric positive-definite
                        (band) matrix *)

open Format
open Slap.Io
open Slap.D
open Slap.Common

let () =
  (* a 5-by-5 symmetric positive-definite band matrix *)
  let a = Mat.of_list_dyn Slap.Size.five Slap.Size.five
                          [[ 1.; 1.; 0.; 0.; 0.];
                           [ 1.; 2.;-1.; 0.; 0.];
                           [ 0.;-1.; 4.; 2.; 0.];
                           [ 0.; 0.; 2.; 3.;-4.];
                           [ 0.; 0.; 0.;-4.;12.]] in
  let b = Mat.random Slap.Size.five Slap.Size.two in

  printf "Solve a * x = b where@.";
  printf "  a = @[[%a]@]@." pp_fmat a;
  printf "  b = @[[%a]@]@.@." pp_fmat b;

  (* posv *)
  let a' = lacpy a in
  let x1 = lacpy b in
  posv a' x1;
  printf "posv: x = @[[%a]@]@." pp_fmat x1;
  printf "      a*x = @[[%a]@]@." pp_fmat (gemm ~transa:normal a
                                                ~transb:normal x1);

  (* ppsv *)
  let ap = Mat.packed a in
  let x2 = lacpy b in
  ppsv ap x2;
  printf "ppsv: x = @[[%a]@]@." pp_fmat x2;
  printf "      a*x = @[[%a]@]@." pp_fmat (gemm ~transa:normal a
                                                ~transb:normal x2);

  (* pbsv *)
  let ab = Mat.syband_dyn Slap.Size.one a in
  let x3 = lacpy b in
  pbsv ~kd:Slap.Size.one ab x3;
  printf "pbsv: x = @[[%a]@]@." pp_fmat x3;
  printf "      a*x = @[[%a]@]@." pp_fmat (gemm ~transa:normal a
                                                ~transb:normal x3)
