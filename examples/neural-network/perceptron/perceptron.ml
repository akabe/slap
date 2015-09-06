(* Perceptron --- A classic single-layer neural network *)

open Format
open Slap.Io
open Slap.Size
open Slap.Common
open Slap.D

let train w x t =
  if t *. (dot x w) < 0.0
  then axpy ~alpha:t x w

let main () =
  let input_dim = three in
  let make_input x1 x2 = Vec.of_list_dyn input_dim [x1; x2; 1.0] in
  (* training set (a target must be +1.0 or -1.0) *)
  let samples = [|
      (make_input 0.0 0.0, 1.0);
      (make_input 1.0 0.0, 1.0);
      (make_input 0.0 1.0, -1.0);
      (make_input 1.0 1.0, -1.0);
    |] in
  (* Create a weight vector *)
  let w = Vec.random input_dim in
  (* training *)
  for _ = 1 to 1000 do
    let k = Random.int (Array.length samples) in
    let (x, t) = samples.(k) in
    train w x t
  done;
  (* Show results of training *)
  printf "w = @[[ %a]@]@." pp_rfvec w;
  Array.iteri (fun i (x, t) ->
               printf "[%d] x = @[[ %a]@], t = %b, y = %b@."
                      i pp_rfvec x (t > 0.0) (dot x w > 0.0))
              samples

let () = main ()
