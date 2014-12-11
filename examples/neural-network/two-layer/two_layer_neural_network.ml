(* A two-layer neural network and online backpropagation *)

open Format
open Slap.Io
open Slap.Size
open Slap.Common
open Slap.D

let sigm a = 1.0 /. (1.0 +. exp (~-. a))
let sigmv v = Vec.map sigm v
let sigmdv v =
  let ones = Vec.make1 (Vec.dim v) in
  Vec.mul v (Vec.sub ones v)

(** Execute a two-layer neural network. *)
let exec w1 w2 b1 b2 x =
  let y = sigmv (gemv ~trans:normal w1 x ~beta:1.0 ~y:(Vec.copy b1)) in (* y := sigmv(w1 * x + b1) *)
  let z = sigmv (gemv ~trans:normal w2 y ~beta:1.0 ~y:(Vec.copy b2)) in (* z := sigmv(w2 * y + b2) *)
  z

(** [check_gradient delta1 w1 w2 b1 b2 x t] checks whether an error [delta1] is
    correct or not by comparison with results of naive numerical
    differentiation. This routine is only for checking implementation.
    The numerical differentiation is much slower than back propagation.

    cf. http://ufldl.stanford.edu/wiki/index.php/Gradient_checking_and_advanced_optimization
*)
let check_gradient delta1 w2 b2 w1 b1 x t =
   let epsilon = 1e-4 in
   let check_4digits dE1 dE2 = (* Check 4 significant digits *)
     let abs_dE1 = abs_float dE1 in
     if abs_dE1 < 1e-9
     then abs_float dE2 < 1e-9 (* true if both `dE1' and `dE2' are nealy zero *)
     else begin
         let diff = (dE1 -. dE2) *. (0.1 ** (floor (log10 abs_dE1) +. 1.0)) in
         abs_float diff < epsilon (* true if 4 significant digits are the same *)
       end
   in
   let check_gradient dE1 calc_eps_error =
     let e_p = calc_eps_error (~+. epsilon) in     (* an error with a gap +epsilon *)
     let e_n = calc_eps_error (~-. epsilon) in     (* an error with a gap -epsilon *)
     let dE2 = (e_p -. e_n) /. (2.0 *. epsilon) in (* dE/dw by naive numerical diff. *)
     if not (check_4digits dE1 dE2)
     then failwith (sprintf "dE1 = %.16f, dE2 = %.16f" dE1 dE2)
   in
   let calc_error_w1 i j eps =
     let elm = Mat.get_dyn w1 i j in  (* elm := w1[i,j] *)
     Mat.set_dyn w1 i j (elm +. eps); (* w1[i,j] := elm + eps *)
     let z = exec w1 w2 b1 b2 x in    (* Compute output of a NN *)
     Mat.set_dyn w1 i j elm;          (* w1[i,j] := elm *)
     (Vec.ssqr_diff z t) /. 2.0       (* return ||z - t||^2 (L2-norm) *)
   in
   let calc_error_b1 i eps =
     let elm = Vec.get_dyn b1 i in  (* elm := b1[i] *)
     Vec.set_dyn b1 i (elm +. eps); (* b1[i] := elm + eps *)
     let z = exec w1 w2 b1 b2 x in  (* Compute output of a NN *)
     Vec.set_dyn b1 i elm;          (* b1[i] := elm *)
     (Vec.ssqr_diff z t) /. 2.0     (* return ||z - t||^2 (L2-norm) *)
   in
   (* Check the gradient [delta1] for [b1] *)
   Vec.iteri (fun i a -> check_gradient a (calc_error_b1 i)) delta1;
   (* Check the gradient [dw1] for [w1] *)
   let dw1 = ger delta1 x (Mat.make0 (Mat.dim1 w1) (Mat.dim2 w1)) in
   for i = 1 to to_int (Mat.dim1 dw1) do
     for j = 1 to to_int (Mat.dim2 dw1) do
       check_gradient (Mat.get_dyn dw1 i j) (calc_error_w1 i j)
     done
   done

(** [train eta w1 w2 b1 b2 x y] updates given weight matrices [w1] and [w2], and
    bias vectors [b1] and [b2] by online back propagation. [x] is an input
    vector, [t] is a target vector and [eta] is a learning rate. [w1], [w2],
    [b1] and [b2] are destructively modified by a call of this routine.
*)
let train eta w1 w2 b1 b2 x t =
  (* Feed-forward propagation *)
  let y = sigmv (gemv ~trans:normal w1 x ~beta:1.0 ~y:(Vec.copy b1)) in (* y := sigmv(w1 * x + b1) *)
  let z = sigmv (gemv ~trans:normal w2 y ~beta:1.0 ~y:(Vec.copy b2)) in (* z := sigmv(w2 * y + b2) *)
  (* Feedback propagation *)
  let delta2 = Vec.mul (Vec.sub z t) (sigmdv z) in                 (* delta1 := (z - t) .* sigmdv(z) *)
  let delta1 = Vec.mul (gemv ~trans:trans w2 delta2) (sigmdv y) in (* delta2 := (w2^T * delta2) .* sigmdv(y) *)
  (* Gradient checking *)
  check_gradient delta1 w2 b2 w1 b1 x t;
  (* Update parameters *)
  ignore (ger ~alpha:(~-. eta) delta2 y w2); (* w2 := w2 - eta * delta2 * y^T *)
  ignore (ger ~alpha:(~-. eta) delta1 x w1); (* w1 := w1 - eta * delta1 * x^T *)
  axpy ~alpha:(~-. eta) b2 ~x:delta2;        (* b2 := b2 - eta * delta2 *)
  axpy ~alpha:(~-. eta) b1 ~x:delta1         (* b1 := b1 - eta * delta1 *)

let main () =
  Random.self_init (); (* Initialize a pseudorandom number generator *)
  let input_dim  = two in  (* the number of units in the input layer *)
  let hidden_dim = five in (* the number of units in the hidden layer *)
  let output_dim = one in  (* the number of units in the output layer *)
  (* a training set (XOR pattern) *)
  let samples = [|
      (Vec.of_array_dyn input_dim [|1.0; 1.0|], Vec.make output_dim 0.0);
      (Vec.of_array_dyn input_dim [|1.0; 0.0|], Vec.make output_dim 1.0);
      (Vec.of_array_dyn input_dim [|0.0; 1.0|], Vec.make output_dim 1.0);
      (Vec.of_array_dyn input_dim [|0.0; 0.0|], Vec.make output_dim 0.0);
     |] in
  (* Create parameters with random values *)
  let w1 = Mat.random hidden_dim input_dim in
  let b1 = Vec.random hidden_dim in
  let w2 = Mat.random output_dim hidden_dim in
  let b2 = Vec.random output_dim in
  (* Training *)
  for i = 0 to 5000 do
    let (x, t) = samples.(i mod (Array.length samples)) in
    train 0.5 w1 w2 b1 b2 x t
  done;
  (* Show results *)
  printf "w1 = @[[ %a]@]@." pp_fmat w1;
  printf "w2 = @[[ %a]@]@." pp_fmat w2;
  printf "b1 = @[[ %a]@]@." pp_rfvec b1;
  printf "b2 = @[[ %a]@]@." pp_rfvec b2;
  Array.iter (fun (x, t) ->
              let z = exec w1 w2 b1 b2 x in
              printf "x = @[[ %a]@]; t = @[[ %a]@]; z = @[[ %a]@]@."
                     pp_rfvec x pp_rfvec t pp_rfvec z) samples

let () = main ()
