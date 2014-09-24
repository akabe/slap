(* symv_sbmv.ml -- Multiplication of a symmetric (band) matrix and a vector *)

open Format
open Slap.Io
open Slap.D
open Slap.Common

let () =
  (* a 5-by-5 symmetric band matrix with two subdiagonals and superdiangonals *)
  let a = Mat.of_list_dyn Slap.Size.five Slap.Size.five
                          [[ 1.; 5.;-4.; 0.; 0.];
                           [ 5.;-1.; 3.; 2.; 0.];
                           [-4.; 3.;-2.;-1.; 4.];
                           [ 0.; 2.;-1.; 0.; 1.];
                           [ 0.; 0.; 4.; 1.; 5.]] in
  let x = Vec.random Slap.Size.five in

  (* symv multiplies a symmetric matrix and a vector *)
  printf "symv: a * x = @[[%a]@]@." pp_rfvec (symv a x);

  (* sbmv multiplies a symmetric *band* matrix and a vector  *)
  let ab = Mat.syband_dyn Slap.Size.two a in (* convert to band storage *)
  printf "sbmv: a * x = @[[%a]@]@." pp_rfvec (sbmv ~k:Slap.Size.two ab x)
