(* gemv_gbmv.ml -- Multiplication of a general (band) matrix and a vector *)

open Format
open Slap.Io
open Slap.D
open Slap.Common

let () =
  (* a 4-by-5 band matrix with two subdiagonals and one superdiangonal *)
  let a = Mat.of_list_dyn Slap.Size.four Slap.Size.five
                          [[ 1.; 4.; 0.; 0.; 0.];
                           [ 5.;-1.; 3.; 0.; 0.];
                           [-4.; 1.;-2.;-1.; 0.];
                           [ 0.;-3.;-1.; 0.; 1.]] in
  let x = Vec.random Slap.Size.five in
  let y = Vec.random Slap.Size.four in

  (* gemv multiplies a general matrix and a vector *)
  printf "gemv: a * x = @[[%a]@]@." pp_rfvec (gemv ~trans:normal a x);

  (* gbmv multiplies a general *band* matrix and a vector  *)
  let ab = Mat.geband_dyn Slap.Size.two Slap.Size.one a in (* convert to band storage *)
  printf "gbmv: a * x = @[[%a]@]@." pp_rfvec (gbmv ~m:Slap.Size.four
                                                   ~trans:normal
                                                   ab
                                                   Slap.Size.two
                                                   Slap.Size.one
                                                   x);

  printf "gemv: a^T * y = @[[%a]@]@." pp_rfvec (gemv ~trans:trans a y);
  printf "gbmv: a^T * y = @[[%a]@]@." pp_rfvec (gbmv ~m:Slap.Size.four
                                                     ~trans:trans
                                                     ab
                                                     Slap.Size.two
                                                     Slap.Size.one
                                                     y);
