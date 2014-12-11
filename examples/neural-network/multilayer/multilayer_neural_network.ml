open Format
open Slap.Io
open Slap.Size
open Slap.D

module NN = NeuralNetwork

let gen_gaussian_samples n klass (xmean, ymean) stddev =
  let gen_point _ =
    let r = sqrt (-2.0 *. log (Random.float 1.0)) in
    let t = 2.0 *. 3.14159265358979 *. Random.float 1.0 in
    let z1 = (r *. cos t) *. stddev +. xmean in
    let z2 = (r *. sin t) *. stddev +. ymean in
    (Vec.of_array_dyn three [|z1; z2; 1.0|], Vec.make one klass)
  in
  Array.init n gen_point

let gen_samples () =
  let (@@) = Array.append in
  let dev = 6.0 in
     (gen_gaussian_samples 50 0.0 ( 10.0,  10.0) dev)
  @@ (gen_gaussian_samples 50 1.0 (-10.0,  10.0) dev)
  @@ (gen_gaussian_samples 50 1.0 ( 10.0, -10.0) dev)
  @@ (gen_gaussian_samples 50 0.0 (-10.0, -10.0) dev)

let get_rms_error nnet samples =
  let e = NN.get_error nnet samples in
  sqrt (2.0 *. e /. (float (Array.length samples)))

let main () =
  Random.self_init ();
  let training_set = gen_samples () in
  let test_set = gen_samples () in
  let module N = (val of_int_dyn 20 : SIZE) in
  let nnet = NN.create ~dropout:0.5 NN.Sigmoid one N.value
             |> NN.append_layer ~dropout:0.5 NN.Sigmoid N.value
             |> NN.append_layer ~dropout:0.5 NN.Sigmoid N.value
             |> NN.append_layer ~dropout:0.2 NN.Sigmoid two in
  NN.init_weights nnet;
  for i = 1 to 50000 do
    NN.minibatch_train ~eta:0.5 ~check:false nnet 20 training_set;
    if i mod 1000 = 0
    then printf "Loop #%d: training error = %g, test error = %g@."
                i (get_rms_error nnet training_set)
                (get_rms_error nnet test_set)
  done

let () = main ()
