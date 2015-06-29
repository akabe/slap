(** Simple Perceptron *)

open Format
open Slap.D
open Slap.Io

let make_weight = function
  | [] -> invalid_arg "perceptron: empty data set"
  | (x,_)::_ -> Vec.make (Vec.dim x) 1.0

let update_weight w rho data =
  let update_w flag (x, y) =
    let y' = dot x w > 0.0 in
    if y = y' then flag
      else begin (* The current prediction y' is wrong. *)
        axpy ~alpha:(if y then rho else ~-.rho) x w;
        true
      end in
  List.fold_left update_w false data (* Returns true if w is modified. *)

let perceptron rho data =
  let w = make_weight data in
  while update_weight w rho data do () done;
  w

let check_result w l =
  List.for_all (fun (x, y) -> let y' = dot w x > 0.0 in y = y') l

let main () =
  let module N = Slap.Size.Of_int_dyn(struct let value = 3 end) in
  let data = [
      (Vec.of_list_dyn N.value [0.0; 0.0; 1.], true);
      (Vec.of_list_dyn N.value [0.0; 1.0; 1.], true);
      (Vec.of_list_dyn N.value [1.0; 0.0; 1.], true);
      (Vec.of_list_dyn N.value [0.6; 0.6; 1.], false);
      (Vec.of_list_dyn N.value [0.0; 2.0; 1.], false);
      (Vec.of_list_dyn N.value [2.0; 0.0; 1.], false)] in
  let w = perceptron 1.0 data in
  let ok = check_result w data in
  printf "w = [%a] : %s\n" pp_rfvec w (if ok then "Success!" else "Failure!")

let _ = main ()
