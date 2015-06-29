(** Principal Component Analysis *)

open Format
open Lacaml.D
open Lacaml.Io

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
  let p = Lacaml.Common.create_int_vec n in
  Vec.sort ~decr:true ~p w;
  Mat.init_rows n k (fun i j -> sigma.{i,p.{j}})

let pca k data =
  let n = Vec.dim (List.hd data) in
  let mu = calc_meanvec n data in
  let sigma = calc_covmat n mu data in
  let u = calc_compress_mat k n sigma in
  let cmp_data = List.map (gemv ~trans:`T u) data in
  u, cmp_data

let main () =
  let k = 1 in
  let data = [
      Vec.of_list [0.01; 0.00;-0.01];
      Vec.of_list [1.00; 1.01; 0.09];
      Vec.of_list [2.01; 2.01; 1.99];
      Vec.of_list [3.00; 2.99; 3.00];
      Vec.of_list [4.01; 4.00; 4.00];
      Vec.of_list [5.00; 5.01; 4.99] ] in
  let u, cmp_data = pca k data in
  printf "U =\n%!%a\n" pp_fmat u;
  List.iter2 (fun x y ->
              printf "vector: [%a] => [%a]%!\n" pp_rfvec x pp_rfvec y
             ) data cmp_data

let _ = main ()
