(* Sized Linear Algebra Package (SLAP)

   Copyright (C) 2013- Akinori ABE <abe@kb.ecei.tohoku.ac.jp>

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

let cnt = PMat.cnt

(** {2 Creation of matrices} *)

let empty = (0, 0, 1, 1, I.Mat.empty)

let create m n = PMat.create prec m n

let make m n x = PMat.make prec m n x

let make0 m n = make m n zero

let make1 m n = make m n one

let identity n = (n, n, 1, 1, I.Mat.identity n)

let init_cols m n f = PMat.init_cols prec m n f

let init_rows m n f = PMat.init_rows prec m n f

let init = init_cols

(** {2 Accessors} *)

let dim = PMat.dim

let dim1 = PMat.dim1

let dim2 = PMat.dim2

let get_dyn = PMat.get_dyn

let set_dyn = PMat.set_dyn

let unsafe_get = PMat.unsafe_get

let unsafe_set = PMat.unsafe_set

let col_dyn = PMat.col_dyn

let row_dyn = PMat.row_dyn

let copy_row_dyn mat i =
  let n, ofsx, incx, x = row_dyn mat i in
  ignore (I.copy ~n ~ofsx ~incx x);
  (n, ofsx, incx, x)

let diag = PMat.diag

let copy_diag mat =
  let n, ofsx, incx, x = diag mat in
  ignore (I.copy ~n ~ofsx ~incx x);
  (n, ofsx, incx, x)

let as_vec = PMat.as_vec

(** {2 Basic operations} *)

let fill = PMat.fill

let copy ?uplo ?b (m, n, ar, ac, a) =
  let br, bc, b = PMat.opt_mat_alloc prec m n b in
  if m <> 0 && n <> 0
  then ignore (I.lacpy ?uplo ~m ~n ~br ~bc ~b ~ar ~ac a);
  (m, n, br, bc, b)

let of_col_vecs_dyn m n vec_array =
  let convert (m', ofsx, incx, x) =
    assert(m = m');
    if not (PVec.check_cnt m' ofsx incx x) then
      invalid_arg "Mat.of_col_vecs_dyn";
    x
  in
  if n <> Array.length vec_array then invalid_arg "Mat.of_col_vecs_dyn";
  let mat = I.Mat.of_col_vecs (Array.map convert vec_array) in
  (m, n, 1, 1, mat)

(** {2 Type conversion} *)

let to_array = PMat.to_array

let of_array_dyn m n array = PMat.of_array_dyn prec m n array

module Of_array (X : sig val value : num_type array array end) : CNTMAT =
  struct
    type m and n
    let value =
      match PMat.dim_array_array X.value with
      | None -> invalid_arg "Mat.Of_array_dyn"
      | Some (m, n) -> PMat.unsafe_of_array prec m n X.value
  end

let of_array aa =
  let module M = Of_array(struct let value = aa end) in
  (module M : CNTMAT)

let to_list = PMat.to_list

let of_list_dyn m n list = PMat.of_list_dyn prec m n list

module Of_list (X : sig val value : num_type list list end) : CNTMAT =
  struct
    type m and n
    let value =
      match PMat.dim_list_list X.value with
      | None -> invalid_arg "Mat.Of_list_dyn"
      | Some (m, n) -> PMat.unsafe_of_list prec m n X.value
  end

let of_list ll =
  let module M = Of_list(struct let value = ll end) in
  (module M : CNTMAT)

(** {2 Iterators} *)

let map f ?b (m, n, ar, ac, a) =
  let br, bc, b = PMat.opt_mat_alloc prec m n b in
  let _ = I.Mat.map f ~m ~n ~br ~bc ~b ~ar ~ac a in
  (m, n, br, bc, b)

let mapi = PMat.mapi prec

let fold_left = PMat.fold_left

let fold_lefti = PMat.fold_lefti

let fold_right = PMat.fold_right

let fold_righti = PMat.fold_righti

let fold_top = PMat.fold_top

let fold_topi = PMat.fold_topi

let fold_bottom = PMat.fold_bottom

let fold_bottomi = PMat.fold_bottomi

let replace_all = PMat.replace_all

let replace_alli = PMat.replace_alli

(** {2 Matrix transformations} *)

let transpose_copy (m, n, ar, ac, a) (n', m', br, bc, b) =
  assert(m = m' && n = n');
  I.Mat.transpose_copy ~m ~n ~ar ~ac a ~br ~bc b

let transpose (m, n, ar, ac, a) =
  let b = I.Mat.transpose ~m ~n ~ar ~ac a in
  (n, m, 1, 1, b)

let detri ?up (n, n', ar, ac, a) =
  assert(n = n');
  I.Mat.detri ?up ~n ~ar ~ac a

let packed = PMat.packed

let unpacked ?up ?(fill_num = Some zero) = PMat.unpacked ?up ~fill_num

let geband_dyn = PMat.geband_dyn

let ungeband m kl ku ?(fill_num = Some zero) =
  PMat.ungeband m kl ku ~fill_num

let syband_dyn = PMat.syband_dyn

let unsyband kd ?up ?(fill_num = Some zero) =
  PMat.unsyband kd ?up ~fill_num

let luband_dyn = PMat.luband_dyn

let unluband m kl ku ?(fill_num = Some zero) =
  PMat.unluband m kl ku ~fill_num

(** {2 Arithmetic operations} *)

let add_const c ?b (m, n, ar, ac, a) =
  let br, bc, b = PMat.opt_mat_alloc prec m n b in
  ignore (I.Mat.add_const c ~m ~n ~br ~bc ~b ~ar ~ac a);
  (m, n, br, bc, b)

let sum (m, n, ar, ac, a) =
  I.Mat.sum ~m ~n ~ar ~ac a

let trace a =
  let n, ofsx, incx, x = diag a in
  I.Vec.sum ~n ~ofsx ~incx x

let scal alpha (m, n, ar, ac, a) =
  I.Mat.scal ~m ~n alpha ~ar ~ac a

let scal_cols (m, n', ar, ac, a) (n, ofsx, incx, x) =
  assert(n = n' && incx = 1);
  I.Mat.scal_cols ~m ~n ~ar ~ac a ~ofs:ofsx x

let scal_rows (m, ofsx, incx, x) (m', n, ar, ac, a) =
  assert(m = m' && incx = 1);
  I.Mat.scal_rows ~m ~n ~ofs:ofsx x ~ar ~ac a

let axpy ?alpha ~x:(m, n, xr, xc, x) (m', n', yr, yc, y) =
  assert(m = m' && n = n');
  I.Mat.axpy ~m ~n ?alpha ~xr ~xc ~x ~yr ~yc y

let gemm_diag ?beta ?y ~transa ?alpha (an, ak, ar, ac, a)
              ~transb (bk, bn, br, bc, b) =
  let n, k = Common.get_transposed_dim transa an ak in
  assert((k, n) = Common.get_transposed_dim transb bk bn);
  let ofsy, incy, y = PVec.opt_vec_alloc prec n y in
  assert(PVec.check_cnt n ofsy incy y);
  ignore (I.Mat.gemm_diag ~n ~k ?beta ~y
                          ~transa:(lacaml_trans3 transa) ?alpha ~ar ~ac a
                          ~transb:(lacaml_trans3 transb) ~br ~bc b);
  (n, ofsy, incy, y)

let syrk_diag ?beta ?y ~trans ?alpha (an, ak, ar, ac, a) =
  let n, k = Common.get_transposed_dim trans an ak in
  let ofsy, incy, y = PVec.opt_vec_alloc prec n y in
  assert(PVec.check_cnt n ofsy incy y);
  ignore (I.Mat.syrk_diag ~n ~k ?beta ~y
                          ~trans:(Common.lacaml_trans2 trans)
                          ?alpha ~ar ~ac a);
  (n, ofsy, incy, y)

let gemm_trace ~transa (an, ak, ar, ac, a) ~transb (bk, bn, br, bc, b) =
  let n, k = Common.get_transposed_dim transa an ak in
  assert((k, n) = Common.get_transposed_dim transb bk bn);
  I.Mat.gemm_trace ~n ~k
                   ~transa:(lacaml_trans3 transa) ~ar ~ac a
                   ~transb:(lacaml_trans3 transb) ~br ~bc b

let syrk_trace (n, k, ar, ac, a) = I.Mat.syrk_trace ~n ~k ~ar ~ac a

let symm2_trace ?upa (n, n', ar, ac, a) ?upb (n'', n''', br, bc, b) =
  assert(n = n' && n = n'' && n = n''');
  I.Mat.symm2_trace ~n ?upa ~ar ~ac a ?upb ~br ~bc b

(** {2 Submatrices} *)

let submat_dyn = PMat.submat_dyn
