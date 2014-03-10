(** K-means clustering *)

open Format
open Slap.D
open Slap.Io
open Slap.Common

(* list mapi *)
let mapi f l =
  let f' (acc, i) x = ((f i x) :: acc, i + 1) in
  let l', _ = List.fold_left f' ([], 0) l in
  List.rev l'

let calc_means k n labels data =
  let sums = Array.init k (fun _ -> (ref 0, Vec.make0 n)) in
  let add_to_sum label x =
    let (count, sum) = sums.(label) in
    axpy x sum;
    count := !count + 1 in
  let div_sum (count, sum) =
    scal (1. /. (float_of_int !count)) sum;
    sum
  in
  List.iter2 add_to_sum labels data;
  Array.map div_sum sums

let reclassify means data =
  let calc_new_label x =
    let update_min (i, min_i, min_d) m =
      let d = Vec.ssqr_diff x m in (* ||m - x|| *)
      if d < min_d then (i + 1, i, d) else (i + 1, min_i, min_d) in
    let _, label, _ = Array.fold_left update_min (0, -1, max_float) means in
    label
  in
  List.map calc_new_label data

let kmeans k data =
  assert(k > 0 && List.length data >= k);
  let n = Vec.dim (List.hd data) in
  let rec loop labels =
    let means = calc_means k n labels data in
    let labels' = reclassify means data in
    if labels = labels' then labels else loop labels'
  in
  let labels0 = mapi (fun i _ -> i mod k) data in
  loop labels0

let main () =
  let module N = Slap.Size.Of_int_dyn(struct let value = 3 end) in
  let data =
    [ Vec.of_list_dyn N.value [ 1.0; 1.0; 1.0];
      Vec.of_list_dyn N.value [ 1.1; 1.3; 0.8];
      Vec.of_list_dyn N.value [ 0.9; 0.7; 1.2];
      Vec.of_list_dyn N.value [ 0.1; 0.1; 0.2];
      Vec.of_list_dyn N.value [ 0.0; 0.1;-0.3];
      Vec.of_list_dyn N.value [-0.1;-0.2; 0.1] ] in
  let labels = kmeans 2 data in
  List.iter2 (fun lb x ->
              printf "label=%d, vector=[%a]\n%!" lb pp_rfvec x
             ) labels data

let _ = main ()
