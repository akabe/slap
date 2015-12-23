(** Steepest descent method with bisection search of learning rates by
    Wolfe conditions

    This implementation is explained at
    http://akabe.github.io/slap/demo-gradopt.html#bisection-search-of-learning-rate-by-wolfe-conditions *)

open Format
open Slap.D
open Slap.Io

(** [wolfe_search ?c1 ?c2 ?init df f p x] executes bisection search of a
    suitable learning rate by Wolfe conditions.
    - [c1] and [c2] are constants for Wolfe conditions,
    - [init] is the initial value of the bisection search,
    - [df] is the derivative of target function [f],
    - [f] is a target function,
    - [p] is a search direction, and
    - [x] is the currect point. *)
let wolfe_search ?(c1=1e-4) ?(c2=0.9) ?(init=1.0) df f p x =
  let middle lo hi = 0.5 *. (lo +. hi) in
  let upper alpha = function (* Compute a new upper bound *)
    | None -> 2.0 *. alpha
    | Some hi -> middle alpha hi
  in
  let q = dot p (df x) in
  let y = f x in
  let xap = Vec.create (Vec.dim x) in
  let rec aux lo hi alpha =
    ignore (copy ~y:xap x); (* xap := x *)
    axpy ~alpha p xap; (* xap := xap + alpha * p *)
    if f xap > y +. c1 *. alpha *. q then aux lo (Some alpha) (middle lo alpha)
    else if dot p (df xap) < c2 *. q then aux alpha hi (upper alpha hi)
    else alpha (* Both of two conditions are satisfied. *)
  in
  aux 0.0 None init

(** [steepest_descent_wolfe ~loops df f x] performs steepest descent method
    with bisection search of learning rates by Wolfe conditions.
    - [loops] is the number of iterations,
    - [df] is the derivative of target function [f],
    - [f] is a target function, and
    - [x] is the initial point of iteration. *)
let steepest_descent_wolfe ~loops df f x =
  for i = 1 to loops do
    let p = Vec.neg (df x) in
    let eta = wolfe_search df f p x in (* Obtain a suitable learning rate *)
    axpy ~alpha:eta p x;
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
  steepest_descent_wolfe ~loops:60 (dgauss a b) (gauss a b) [%vec [0.; 1.]]
