(** Newton method

    This implementation is explained at
    http://akabe.github.io/slap/demo-gradopt.html#newton-method *)

open Format
open Slap.D
open Slap.Io

(** [newton ~loops ~eta ddf df f x] performs steepest descent method.
    - [loops] is the number of iterations,
    - [eta] is a learning rate,
    - [ddf] is the second derivative of target function [f],
    - [df] is the first derivative of [f],
    - [f] is a target function, and
    - [x] is the initial point of iteration. *)
let newton ~loops ~eta ddf df f x =
  for i = 1 to loops do
    let h = ddf x in
    sytri h; (* h := h^(-1) *)
    ignore (symv ~alpha:(~-. eta) h (df x) ~beta:1. ~y:x); (* x := x - eta * h * (df x) *)
    printf "Loop %d: f = %g, x = @[%a@]@." i (f x) pp_rfvec x
  done

(** [gauss a b x] computes a Gaussian function. [a] is a symmetric matrix, and
    [b] is a vector. *)
let gauss a b x =
  let xb = Vec.sub x b in
  ~-. (exp (dot xb (symv a xb) /. 2.0)) (* -exp( (x-b)^T * a * (x-b) / 2 ) *)

(** The first derivative of a Gaussian function. *)
let dgauss a b x = symv ~alpha:(gauss a b x) a (Vec.sub x b)

(** The second derivative of a Gaussian function. *)
let ddgauss a b x =
  let a' = Mat.copy a in
  ignore (syr (symv a (Vec.sub x b)) a'); (* a' := a' + a * (x-b) * (x-b)^T * a^T *)
  Mat.scal (gauss a b x) a';
  a'

let () =
  let a = [%mat [-0.1, 0.1;
                 0.1, -0.2]] in
  let b = [%vec [1.0; 3.0]] in
  newton ~loops:20 ~eta:0.4 (ddgauss a b) (dgauss a b) (gauss a b)
    [%vec [0.; 1.]]
