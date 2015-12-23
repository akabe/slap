(** Steepest descent method

    This implementation is explained at
    http://akabe.github.io/slap/demo-gradopt.html#steepest-descent-method *)

open Format
open Slap.D
open Slap.Io

(** [steepest_descent ~loops ~eta df f x] performs steepest descent method.
    - [loops] is the number of iterations,
    - [eta] is a learning rate,
    - [df] is the derivative of target function [f],
    - [f] is a target function, and
    - [x] is the initial point of iteration. *)
let steepest_descent ~loops ~eta df f x =
  for i = 1 to loops do
    axpy ~alpha:(~-. eta) (df x) x; (* x := x - eta * (df x) *)
    printf "Loop %d: f = %g, x = @[%a@]@." i (f x) pp_rfvec x
  done

(** [gauss a b x] computes a Gaussian function. [a] is a symmetric matrix, and
    [b] is a vector. *)
let gauss a b x =
  let xb = Vec.sub x b in
  ~-. (exp (dot xb (symv a xb) /. 2.0)) (* -exp( (x-b)^T * a * (x-b) / 2 ) *)

(** The derivative of a Gaussian function. *)
let dgauss a b x = symv ~alpha:(gauss a b x) a (Vec.sub x b)

let () =
  let a = [%mat [-0.1, 0.1;
                 0.1, -0.2]] in
  let b = [%vec [1.0; 3.0]] in
  steepest_descent ~loops:100 ~eta:2. (dgauss a b) (gauss a b) [%vec [0.; 1.]]
