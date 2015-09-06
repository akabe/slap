open Format
open Slap.Io
open Slap.Common
open Slap.D

module Size = Slap.Size

type actv =
  | Sigmoid
  | Tanh

type ('indim, 'loutdim, 'outdim, 'upper) t =
  {
    upper : 'upper;
    input_dim : 'indim Slap.Size.t;
    output_dim : 'loutdim Slap.Size.t;
    actv_f : ('loutdim, Slap.cnt) vec -> ('loutdim, Slap.cnt) vec;
    actv_fd : ('loutdim, Slap.cnt) vec -> ('loutdim, Slap.cnt) vec;
    aug_output : ('loutdim Size.s, Slap.cnt) vec;
    wmat : ('indim Size.s, 'loutdim, Slap.cnt) mat;
    dwmat : ('indim Size.s, 'loutdim, Slap.cnt) mat;
    delta : ('loutdim, Slap.cnt) vec;
    dropout : (float * ('indim, Slap.cnt) vec) option;

    fold : 'acc. ('indim, 'loutdim, 'outdim, 'upper) t ->
           'acc fld -> 'acc -> 'acc;
    exec : ('indim, 'loutdim, 'outdim, 'upper) t -> bool ->
           ('indim Size.s, Slap.cnt) vec -> ('outdim, Slap.cnt) vec;
    feedback : ('indim, 'loutdim, 'outdim, 'upper) t ->
               ('indim Size.s, Slap.cnt) vec -> ('outdim, Slap.cnt) vec -> unit;
  }
and 'acc fld =
  {
    fld : 'indim 'loutdim 'outdim 'upper.
          ('indim, 'loutdim, 'outdim, 'upper) t ->
          'acc -> 'acc
  }

let sigmv ?y x = Vec.map (fun a -> 1.0 /. (1.0 +. exp (~-. a))) ?y x

let sigmdv s =
  let ones = Vec.make1 (Vec.dim s) in (* a one vector *)
  Vec.mul s (Vec.sub ones s) (* return s .* (1 - s) *)

let tanhv ?y x = Vec.map (tanh) ?y x

let tanhdv s =
  let ones = Vec.make1 (Vec.dim s) in (* a one vector *)
  Vec.sub ones (Vec.mul s s) (* return s .* (1 - s) *)

let get_activation_fun = function
  | Sigmoid -> sigmv, sigmdv
  | Tanh -> tanhv, tanhdv

let create_aug_output output_dim =
  let aug_output = Vec.create (Size.succ output_dim) in
  Vec.set_dyn aug_output (Size.to_int output_dim + 1) 1.0; (* for a bias *)
  aug_output

let create_dropout_mask input_dim = function
  | None -> None
  | Some prob -> Some (prob, Vec.create input_dim)

let get_unaug_output nnet = Vec.subcntvec_dyn nnet.output_dim nnet.aug_output

let apply_dropout_mask nnet masking v =
  match nnet.dropout, masking with
  | Some (_, mask), true ->
     ignore (Vec.mul ~z:v v mask);
     1.0
  | Some (prob, _), false -> prob
  | _ -> 1.0

let exec_onestep nnet masking x =
  let alpha = apply_dropout_mask nnet masking
                                 (Vec.subvec_dyn nnet.input_dim x) in
  let y = get_unaug_output nnet in
  ignore (sigmv ~y (gemv ~y ~trans:trans ~alpha nnet.wmat x));
  y

let feedback_output_layer nnet x t =
  let y = get_unaug_output nnet in
  ignore (Vec.mul ~z:nnet.delta (Vec.sub y t) (sigmdv y));
  ignore (ger x nnet.delta nnet.dwmat)

let create ?dropout actv output_dim input_dim =
  let actv_f, actv_fd = get_activation_fun actv in
  {
    input_dim; output_dim; actv_f; actv_fd;
    upper = ();
    aug_output = create_aug_output output_dim;
    wmat = Mat.create (Size.succ input_dim) output_dim;
    dwmat = Mat.create (Size.succ input_dim) output_dim;
    delta = Vec.create output_dim;
    dropout = create_dropout_mask input_dim dropout;
    fold = (fun nnet fld acc -> fld.fld nnet acc);
    exec = exec_onestep;
    feedback = feedback_output_layer;
  }

let feedback_hidden_layer nnet x =
  let y = get_unaug_output nnet in
  let w_up = nnet.upper.wmat in
  let wsub_up = Mat.submat_dyn (Vec.dim y) (Mat.dim2 w_up) w_up in
  ignore (Vec.mul ~z:nnet.delta (gemv ~trans:normal wsub_up nnet.upper.delta)
                  (sigmdv y));
  begin match nnet.upper.dropout with
        | Some (_, mask) -> ignore (Vec.mul ~z:nnet.delta nnet.delta mask)
        | None -> ()
  end;
  ignore (ger x nnet.delta nnet.dwmat)

let append_layer ?dropout actv input_dim upper =
  let actv_f, actv_fd = get_activation_fun actv in
  let output_dim = upper.input_dim in
  let fold nnet fld acc = upper.fold upper fld (fld.fld nnet acc) in
  let exec nnet masking x =
    ignore (exec_onestep nnet masking x);
    upper.exec upper masking nnet.aug_output
  in
  let feedback nnet x t =
    upper.feedback upper nnet.aug_output t;
    feedback_hidden_layer nnet x
  in
  {
    input_dim; output_dim; actv_f; actv_fd; upper;
    aug_output = create_aug_output output_dim;
    wmat = Mat.create (Size.succ input_dim) output_dim;
    dwmat = Mat.create (Size.succ input_dim) output_dim;
    delta = Vec.create output_dim;
    dropout = create_dropout_mask input_dim dropout;
    fold; exec; feedback;
  }

let fold nnet fld init = nnet.fold nnet fld init

let exec nnet x = nnet.exec nnet false x

let init_weights ?(from=(-1.0)) ?(range=2.0) nnet =
  let init_wmat nnet () =
    Mat.replace_all nnet.wmat (fun _ -> Random.float range +. from)
  in
  fold nnet {fld=init_wmat} ()

let get_error nnet samples =
  let add_error acc (x, t) =
    let y = nnet.exec nnet false x in
    acc +. Vec.ssqr_diff y t
  in
  (Array.fold_left add_error 0.0 samples) /. 2.0

let check_gradient nnet samples =
  let epsilon = 1e-4 in
  let get_eps_error eps i j =
    let add_error acc (x, t) =
      let y = nnet.exec nnet true x in
      acc +. Vec.ssqr_diff y t
    in
    let elm = Mat.get_dyn nnet.wmat i j in
    Mat.set_dyn nnet.wmat i j (elm +. eps);
    let err = (Array.fold_left add_error 0.0 samples) /. 2.0 in
    Mat.set_dyn nnet.wmat i j elm;
    err
  in
  let calc_gradient_ij i j =
    let e_p = get_eps_error (~+. epsilon) i j in
    let e_n = get_eps_error (~-. epsilon) i j in
    (e_p -. e_n) /. (2.0 *. epsilon)
  in
  let check_four_significant_digits dE1 dE2 =
    let abs_dE1 = abs_float dE1 in
    if abs_dE1 < 1e-9
    then abs_float dE2 < 1e-9 (* true if both `dE1' and `dE2' are nealy zero *)
    else begin
        let diff = (dE1 -. dE2) *. (0.1 ** (floor (log10 abs_dE1) +. 1.0)) in
        abs_float diff < epsilon (* true if 4 significant digits are the same *)
      end
  in
  (* Check all elements in a gradient matrix. *)
  let r = 1.0 /. float (Array.length samples) in
  for i = 1 to Size.to_int (Mat.dim1 nnet.dwmat) do
    for j = 1 to Size.to_int (Mat.dim2 nnet.dwmat) do
      let dE1 = r *. Mat.get_dyn nnet.dwmat i j in (* dE/dw by back propagation *)
      let dE2 = r *. calc_gradient_ij i j in (* dE/dw by naive numerical diff. *)
      if not (check_four_significant_digits dE1 dE2)
      then failwith (sprintf "dE/dw[%d,%d] = %.16f, but should be %.16f"
                             i j dE1 dE2)
    done
  done

let init_mask nnet =
  let fld nnet () =
    match nnet.dropout with
    | None -> ()
    | Some (prob, mask) ->
       Vec.fill mask 1.0;
       let n = int_of_float (float (Size.to_int nnet.input_dim) *. prob) in
       let d = Size.to_int (Vec.dim mask) in
       let i = ref 0 in
       while !i < n do
         let k = (Random.int d) + 1 in
         if Vec.get_dyn mask k > 1e-9 then begin
             Vec.set_dyn mask k 0.0;
             i := !i + 1
           end
       done
  in
  fold nnet {fld} ()

let init_dwmat nnet =
  let fill0 nnet () = Mat.fill nnet.dwmat 0.0 in
  fold nnet {fld=fill0} ()

let update_wmat nnet eta =
  let alpha = ~-. eta in
  let update nnet () = Mat.axpy ~alpha nnet.dwmat nnet.wmat in
  fold nnet {fld=update} ()

let online_train ?(eta=0.5) ?(check=true) nnet x t =
  init_mask nnet;
  init_dwmat nnet;
  ignore (nnet.exec nnet true x); (* feed forward *)
  nnet.feedback nnet x t; (* feed back *)
  if check then check_gradient nnet [|(x, t)|];
  update_wmat nnet eta

let batch_train ?(eta=0.5) ?(check=true) nnet samples =
  init_mask nnet;
  init_dwmat nnet;
  Array.iter (fun (x, t) ->
              ignore (nnet.exec nnet true x); (* feed forward *)
              nnet.feedback nnet x t; (* feed back *))
             samples;
  if check then check_gradient nnet samples;
  update_wmat nnet eta

let minibatch_train ?(eta=0.5) ?(check=true) nnet n samples =
  init_mask nnet;
  init_dwmat nnet;
  for _ = 1 to n do
    let (x, t) = samples.(Random.int (Array.length samples)) in
    ignore (nnet.exec nnet true x); (* feed forward *)
    nnet.feedback nnet x t; (* feed back *)
  done;
  if check then check_gradient nnet samples;
  update_wmat nnet eta
