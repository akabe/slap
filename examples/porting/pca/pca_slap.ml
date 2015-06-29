(** Principal Component Analysis *)

open Format
open Slap.D
open Slap.Io
open Slap.Common

let calc_meanvec n data =
  let mu = Vec.make0 n in
  List.iter (fun x -> axpy x mu) data;
  scal (1.0 /. (float_of_int (List.length data))) mu;
  mu

let calc_covmat n mu data =
  let cdata = List.map (fun x -> Vec.sub x mu) data in
  let sigma = Mat.make0 n n in
  List.iter (fun x -> ignore (syr x sigma)) cdata;
  sigma

let calc_compress_mat k n sigma =
  let w = syev ~vectors:true sigma in
  let p = create_int_vec n in
  Vec.sort ~decr:true ~p w;
  Mat.init_cols n k (fun i j -> Mat.get_dyn sigma i (Slap.Vec.get_dyn p j))

let pca k data =
  let n = Vec.dim (List.hd data) in
  let mu = calc_meanvec n data in
  let sigma = calc_covmat n mu data in
  let u = calc_compress_mat k n sigma in
  let cmp_data = List.map (gemv ~trans:trans u) data in
  u, cmp_data

let main () =
  let module K = Slap.Size.Of_int_dyn(struct let value = 1 end) in
  let module N = Slap.Size.Of_int_dyn(struct let value = 3 end) in
  let data = [
      Vec.of_list_dyn N.value [0.01; 0.00;-0.01];
      Vec.of_list_dyn N.value [1.00; 1.01; 0.09];
      Vec.of_list_dyn N.value [2.01; 2.01; 1.99];
      Vec.of_list_dyn N.value [3.00; 2.99; 3.00];
      Vec.of_list_dyn N.value [4.01; 4.00; 4.00];
      Vec.of_list_dyn N.value [5.00; 5.01; 4.99] ] in
  let u, cmp_data = pca K.value data in
  printf "U =\n%!%a\n" pp_fmat u;
  List.iter2 (fun x y ->
              printf "vector: [%a] => [%a]%!\n" pp_rfvec x pp_rfvec y
             ) data cmp_data

let _ = main ()
