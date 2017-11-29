(* Sized Linear Algebra Package (SLAP)

   Copyright (C) 2013- Akinori ABE <aabe.65535@gmail.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*)

module type CNTMAT =
  sig
    type m (** A generative phantom type. *)
    type n (** A generative phantom type. *)
    val value : (m, n, 'cnt) mat
  end

module type DSCMAT =
  sig
    type m (** A generative phantom type. *)
    type n (** A generative phantom type. *)
    val value : (m, n, dsc) mat
  end

let cnt = Mat.cnt

(** {2 Creation of matrices} *)

let empty = M.__unexpose Size.zero Size.zero 1 1 I.Mat.empty

let create m n = Mat.create prec m n

let make m n x = Mat.make prec m n x

let make0 m n = make m n zero

let make1 m n = make m n one

let identity n = M.__unexpose n n 1 1 (I.Mat.identity (S.__expose n))

let init_cols m n f = Mat.init_cols prec m n f

let init_rows m n f = Mat.init_rows prec m n f

let init = init_cols

(** {2 Accessors} *)

let dim = Mat.dim

let dim1 = Mat.dim1

let dim2 = Mat.dim2

let get_dyn = Mat.get_dyn

let set_dyn = Mat.set_dyn

let unsafe_get = Mat.unsafe_get

let unsafe_set = Mat.unsafe_set

let col_dyn = Mat.col_dyn

let row_dyn = Mat.row_dyn

let copy_row_dyn a i = Vec.copy (row_dyn a i)

let diag = Mat.diag

let diag_rect = Mat.diag_rect

let copy_diag a = Vec.copy (diag a)

let copy_diag_rect a = Vec.copy (diag_rect a)

let as_vec = Mat.as_vec

(** {2 Basic operations} *)

let fill = Mat.fill

let copy ?uplo ?b a =
  let m, n, ar, ac, a = M.__expose a in
  let br, bc, b = Mat.opt_mat_alloc prec m n b in
  if S.__expose m <> 0 && S.__expose n <> 0
  then ignore (I.lacpy ?uplo ~m:(S.__expose m) ~n:(S.__expose n)
                 ~br ~bc ~b ~ar ~ac a);
  M.__unexpose m n br bc b

let of_col_vecs_dyn m n vec_array =
  let convert x =
    assert(Slap__Vec.check_cnt x);
    let m', _, x = V.__expose x in
    assert(m = m');
    x
  in
  if S.__expose n <> Array.length vec_array
  then invalid_argf "Mat.of_col_vecs_dyn" ();
  let mat = I.Mat.of_col_vecs (Array.map convert vec_array) in
  M.__unexpose m n 1 1 mat

(** {2 Type conversion} *)

let to_array = Mat.to_array

let of_array_dyn m n array = Mat.of_array_dyn prec m n array

module Of_array (X : sig val value : num_type array array end) : CNTMAT =
  struct
    type m and n
    let value =
      match Mat.dim_array_array X.value with
      | None -> invalid_argf "Mat.Of_array_dyn" ()
      | Some (m, n) ->
        Mat.unsafe_of_array prec
          (S.__unexpose m) (S.__unexpose n) X.value
  end

let of_array aa =
  let module M = Of_array(struct let value = aa end) in
  (module M : CNTMAT)

let unsafe_of_array m n aa = Mat.unsafe_of_array prec m n aa

let to_list = Mat.to_list

let of_list_dyn m n ll = Mat.of_list_dyn prec m n ll

module Of_list (X : sig val value : num_type list list end) : CNTMAT =
  struct
    type m and n
    let value =
      match Mat.dim_list_list X.value with
      | None -> invalid_argf "Mat.Of_list_dyn" ()
      | Some (m, n) ->
        Mat.unsafe_of_list prec
          (S.__unexpose m) (S.__unexpose n) X.value
  end

let of_list ll =
  let module M = Of_list(struct let value = ll end) in
  (module M : CNTMAT)

let unsafe_of_list m n ll = Mat.unsafe_of_list prec m n ll

let to_bigarray = Mat.to_bigarray

let of_bigarray_dyn = Mat.of_bigarray_dyn

module Of_bigarray
    (X : sig
       val value : (num_type, prec, fortran_layout) Array2.t
     end) : CNTMAT =
  struct
    type m and n
    let value =
      let m = S.__unexpose (Array2.dim1 X.value) in
      let n = S.__unexpose (Array2.dim2 X.value) in
      Mat.unsafe_of_bigarray m n X.value
  end

let of_bigarray ba =
  let module M = Of_bigarray(struct let value = ba end) in
  (module M : CNTMAT)

let unsafe_of_bigarray = Mat.unsafe_of_bigarray

let of_array_c aa = Mat.of_array_c prec aa

let of_list_c ll = Mat.of_list_c prec ll

let of_bigarray_c ?(share=false) ba = Mat.of_bigarray_c ~share ba

(** {2 Iterators} *)

let map f ?b a = Mat.map prec f ?b a

let mapi f ?b a = Mat.mapi prec f ?b a

let fold_left = Mat.fold_left

let fold_lefti = Mat.fold_lefti

let fold_right = Mat.fold_right

let fold_righti = Mat.fold_righti

let fold_top = Mat.fold_top

let fold_topi = Mat.fold_topi

let fold_bottom = Mat.fold_bottom

let fold_bottomi = Mat.fold_bottomi

let replace_all = Mat.replace_all

let replace_alli = Mat.replace_alli

(** {2 Matrix transformations} *)

let transpose_copy ?b a =
  let m, n, ar, ac, a = M.__expose a in
  let br, bc, b = M.opt_mat_alloc prec n m b in
  ignore (I.Mat.transpose_copy ~m:(S.__expose m) ~n:(S.__expose n)
            ~ar ~ac a ~br ~bc ~b);
  M.__unexpose n m 1 1 b

let detri ?up a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  I.Mat.detri ?up ~n:(S.__expose n) ~ar ~ac a

let packed = Mat.packed

let unpacked ?up ?(fill_num = Some zero) = Mat.unpacked ?up ~fill_num

let geband_dyn = Mat.geband_dyn

let ungeband m kl ku ?(fill_num = Some zero) =
  Mat.ungeband m kl ku ~fill_num

let syband_dyn = Mat.syband_dyn

let unsyband kd ?up ?(fill_num = Some zero) =
  Mat.unsyband kd ?up ~fill_num

let luband_dyn = Mat.luband_dyn

let unluband m kl ku ?(fill_num = Some zero) =
  Mat.unluband m kl ku ~fill_num

(** {2 Arithmetic operations} *)

let wrap1
    (f : ?m:int -> ?n:int -> ?ar:int -> ?ac:int -> I.mat -> _)
    a =
  let m, n, ar, ac, a = M.__expose a in
  f ~m:(S.__expose m) ~n:(S.__expose n) ~ar ~ac a

let wrap2
    (f : ?m:int -> ?n:int ->
     ?br:int -> ?bc:int -> ?b:I.mat ->
     ?ar:int -> ?ac:int -> I.mat -> _)
    ?b a =
  let m, n, ar, ac, a = M.__expose a in
  let br, bc, b = Mat.opt_mat_alloc prec m n b in
  ignore (f ~m:(S.__expose m) ~n:(S.__expose n) ~br ~bc ~b ~ar ~ac a);
  M.__unexpose m n br bc b

let add_const (c:I.num_type) ?b a = wrap2 (I.Mat.add_const c) ?b a

let sum a = wrap1 I.Mat.sum a

let trace a = Vec.sum (diag_rect a)

let scal alpha a =
  wrap1 (fun ?m ?n ?ar ?ac a -> I.Mat.scal alpha ?m ?n ?ar ?ac a) a

let scal_cols a x =
  let m, n', ar, ac, a = M.__expose a in
  let n, incx, x = V.__expose x in
  assert(n = n' && incx = 1);
  I.Mat.scal_cols ~m:(S.__expose m) ~n:(S.__expose n) ~ar ~ac a ~ofs:1 x

let scal_rows x a =
  let m, incx, x = V.__expose x in
  let m', n, ar, ac, a = M.__expose a in
  assert(m = m' && incx = 1);
  I.Mat.scal_rows ~m:(S.__expose m) ~n:(S.__expose n) ~ofs:1 x ~ar ~ac a

let axpy ?alpha x y =
  let m, n, xr, xc, x = M.__expose x in
  let m', n', yr, yc, y = M.__expose y in
  assert(m = m' && n = n');
  I.Mat.axpy ~m:(S.__expose m) ~n:(S.__expose n) ?alpha ~xr ~xc x ~yr ~yc y

let gemm_diag ?beta ?y ~transa ?alpha a ~transb b =
  let an, ak, ar, ac, a = M.__expose a in
  let bk, bn, br, bc, b = M.__expose b in
  let n, k = Common.get_transposed_dim transa an ak in
  assert((k, n) = Common.get_transposed_dim transb bk bn);
  let y = Slap__Vec.opt_cnt_vec_alloc prec n y in
  ignore (I.Mat.gemm_diag ~n:(S.__expose n) ~k:(S.__expose k) ?beta ~y
                          ~transa:(lacaml_trans3 transa) ?alpha ~ar ~ac a
                          ~transb:(lacaml_trans3 transb) ~br ~bc b);
  V.__unexpose n 1 y

let syrk_diag ?beta ?y ~trans ?alpha a =
  let an, ak, ar, ac, a = M.__expose a in
  let n, k = Common.get_transposed_dim trans an ak in
  let y = Slap__Vec.opt_cnt_vec_alloc prec n y in
  ignore (I.Mat.syrk_diag ~n:(S.__expose n) ~k:(S.__expose k) ?beta ~y
                          ~trans:(Common.lacaml_trans2 trans)
                          ?alpha ~ar ~ac a);
  V.__unexpose n 1 y

let gemm_trace ~transa a ~transb b =
  let an, ak, ar, ac, a = M.__expose a in
  let bk, bn, br, bc, b = M.__expose b in
  let n, k = Common.get_transposed_dim transa an ak in
  assert((k, n) = Common.get_transposed_dim transb bk bn);
  I.Mat.gemm_trace ~n:(S.__expose n) ~k:(S.__expose k)
                   ~transa:(lacaml_trans3 transa) ~ar ~ac a
                   ~transb:(lacaml_trans3 transb) ~br ~bc b

let syrk_trace a = wrap1 (fun ?m ?n -> I.Mat.syrk_trace ?n:m ?k:n) a

let symm2_trace ?upa a ?upb b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', n''', br, bc, b = M.__expose b in
  assert(n = n' && n = n'' && n = n''');
  I.Mat.symm2_trace ~n:(S.__expose n) ?upa ~ar ~ac a ?upb ~br ~bc b

(** {2 Submatrices} *)

let submat_dyn = Mat.submat_dyn
