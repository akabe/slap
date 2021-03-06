(* jacobi.ml --- Solve a linear system by Jacobi method

   Compilation:
   $ ocamlfind ocamlopt -package slap -linkpkg -short-paths jacobi.ml
 *)

open Slap.Io
open Slap.Common
open Slap.Size
open Slap.D

let jacobi a b =
  let d_inv = Vec.reci (Mat.diag a) in (* reciprocal diagonal elements *)
  let r = Mat.mapi (fun i j aij -> if i = j then 0.0 else aij) a in
  let y = Vec.create (Vec.dim b) in (* temporary memory *)
  let rec loop z x =
    ignore (copy ~y b); (* y := b *)
    ignore (gemv ~y ~trans:normal ~alpha:(-1.0) ~beta:1.0 r x); (* y := y-r*x *)
    ignore (Vec.mul ~z d_inv y); (* z := element-wise mult. of d_inv and y *)
    if Vec.ssqr_diff x z < 1e-10 then z else loop x z (* Check convergence *)
  in
  let x0 = Vec.make (Vec.dim b) 1.0 in (* the initial values of `x' *)
  let z = Vec.create (Vec.dim b) in (* temporary memory *)
  loop z x0

let () =
  let a = [%mat [5.0, 1.0, 0.0;
                 1.0, 3.0, 1.0;
                 0.0, 1.0, 4.0]] in
  let b = [%vec [7.0; 10.0; 14.0]] in
  let x = jacobi a b in
  Format.printf "a = @[%a@]@.b = @[%a@]@." pp_fmat a pp_rfvec b;
  Format.printf "x = @[%a@]@." pp_rfvec x;
  Format.printf "a*x = @[%a@]@." pp_rfvec (gemv ~trans:normal a x)
