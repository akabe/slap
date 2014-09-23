(* trsv_tpsv.ml -- Solve a system of linear equations by trsv and tpsv. *)

open Format
open Slap.Io
open Slap.D
open Slap.Common

let () =
  let module N = (val Slap.Size.of_int_dyn 6 : Slap.Size.SIZE) in
  (* an (upper triangular) coefficient matrix *)
  let a = Mat.of_list_dyn N.value N.value
                          [[ -6.;  9.; -6.; -3.; -1.;  6.];
                           [  0.; -8.; -4.;  6.;  6.; -3.];
                           [  0.;  0.;  7.; -5.; -9.; -2.];
                           [  0.;  0.;  0.; -6.;  3.;  3.];
                           [  0.;  0.;  0.;  0.;  5.;  9.];
                           [  0.;  0.;  0.;  0.;  0.;  6.]] in
  (* the r.h.s. *)
  let b = Vec.of_list_dyn N.value [1.; -4.; 2.; -3.; 0.; -1.] in
  printf "a = @[[%a]@]@." pp_fmat a;
  printf "b = @[[%a]@]@." pp_rfvec b;

  (* ----- using trsv ----- *)
  let x = copy b in (* copy `b' into `x'. *)
  trsv ~trans:normal a x; (* x := the solution of "a * x = b" *)
  printf "trsv(a * x = b):@.";
  printf "  x   = @[[%a]@]@." pp_rfvec x;
  trmv ~trans:normal a x; (* x := a * x *)
  printf "  a*x = @[[%a]@]@." pp_rfvec x;

  (* ----- using tpsv ----- *)
  let ap = Mat.packed a in (* pack matrix `a'. *)
  let x = copy b in (* copy `b' into `x'. *)
  tpsv ~trans:normal ap x; (* x := the solution of "a * x = b" *)
  printf "tpsv(a * x = b):@.";
  printf "  x   = @[[%a]@]@." pp_rfvec x;
  tpmv ~trans:normal ap x; (* x := a * x *)
  printf "  a*x = @[[%a]@]@." pp_rfvec x
