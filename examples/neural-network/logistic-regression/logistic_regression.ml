(* Logistic regression --- A classic single-layer neural network *)

open Format
open Slap.Io
open Slap.Size
open Slap.Common
open Slap.D

let sigm a = 1.0 /. (1.0 +. exp (~-. a))

let train eta w x t =
  let y = sigm (dot x w) in
  axpy ~alpha:(eta *. (t -. y)) x w

let main () =
  let input_dim = three in
  let make_input x1 x2 = Vec.of_list_dyn input_dim [x1; x2; 1.0] in
  (* training set (a target must be 1.0 or 0.0) *)
  let samples = [|
      (make_input 0.0 0.0, 1.0);
      (make_input 1.0 0.0, 1.0);
      (make_input 0.0 1.0, 0.0);
      (make_input 1.0 1.0, 0.0);
    |] in
  (* Create a weight vector *)
  let w = Vec.random input_dim in
  (* training *)
  for _ = 1 to 1000 do
    let k = Random.int (Array.length samples) in
    let (x, t) = samples.(k) in
    train 0.5 w x t
  done;
  (* Show results of training *)
  printf "w = @[[%a]@]@." pp_rfvec w;
  Array.iteri (fun i (x, t) ->
               printf "[%d] x = @[[%a]@], t = %g, y = %g@."
                      i pp_rfvec x t (sigm (dot x w)))
              samples

let () = main ()
