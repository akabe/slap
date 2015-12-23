(** Quasi-Newton method

    This implementation is explained at
    http://akabe.github.io/slap/demo-gradopt.html#quasi-newton-method *)

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

(** [update_h ?up h y s] destructively updates an approximated inverse Hessian
    matrix by BFGS (Broyden-Fletcher-Goldfarb-Shanno) algorithm. *)
let update_h ?up h y s =
  let rho = 1. /. dot y s in
  let hy = symv ?up h y in
  ignore (ger ~alpha:(~-. rho) hy s h); (* h := h - rho * hy * s^T *)
  ignore (ger ~alpha:(~-. rho) s hy h); (* h := h - rho * s * hy^T *)
  ignore (syr ?up ~alpha:((1. +. dot y hy *. rho) *. rho) s h)

(** [quasi_newton ~loops df f x] performs Quasi-Newton method.
    - [loops] is the number of iterations,
    - [df] is the derivative of target function [f],
    - [f] is a target function, and
    - [x] is the initial point of iteration. *)
let quasi_newton ~loops df f x0 =
  let h = Mat.identity (Vec.dim x0) in (* an approximated inverse Hessian *)
  let rec aux i x df_dx =
    if i <= loops then begin
      let s = symv ~alpha:(-1.) h df_dx in (* search direction *)
      scal (wolfe_search df f s x) s;
      let x' = Vec.add x s in
      let df_dx' = df x' in
      let y = Vec.sub df_dx' df_dx in
      if dot y s > 1e-9 then begin (* Avoid divergence *)
        update_h h y s; (* Update an approximated inverse Hessian *)
        printf "Loop %d: f x = %g, x = @[%a@]@." i (f x) pp_rfvec x;
        aux (i + 1) x' df_dx'
      end
    end
  in
  aux 1 x0 (df x0)

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
  quasi_newton ~loops:10 (dgauss a b) (gauss a b) [%vec [0.0; 1.0]]
