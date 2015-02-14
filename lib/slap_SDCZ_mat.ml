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

let cnt = Slap_mat.cnt

(** {2 Creation of matrices} *)

let empty = __unexpose_mat (Slap_size.zero, Slap_size.zero, 1, 1, I.Mat.empty)

let create m n = Slap_mat.create prec m n

let make m n x = Slap_mat.make prec m n x

let make0 m n = make m n zero

let make1 m n = make m n one

let identity n = __unexpose_mat (n, n, 1, 1, I.Mat.identity (__expose_size n))

let init_cols m n f = Slap_mat.init_cols prec m n f

let init_rows m n f = Slap_mat.init_rows prec m n f

let init = init_cols

(** {2 Accessors} *)

let dim = Slap_mat.dim

let dim1 = Slap_mat.dim1

let dim2 = Slap_mat.dim2

let get_dyn = Slap_mat.get_dyn

let set_dyn = Slap_mat.set_dyn

let unsafe_get = Slap_mat.unsafe_get

let unsafe_set = Slap_mat.unsafe_set

let col_dyn = Slap_mat.col_dyn

let row_dyn = Slap_mat.row_dyn

let copy_row_dyn a i = Slap_vec.copy (row_dyn a i)

let diag = Slap_mat.diag

let diag_rect = Slap_mat.diag_rect

let copy_diag a = Slap_vec.copy (diag a)

let copy_diag_rect a = Slap_vec.copy (diag_rect a)

let as_vec = Slap_mat.as_vec

(** {2 Basic operations} *)

let fill = Slap_mat.fill

let copy ?uplo ?b a =
  let m, n, ar, ac, a = __expose_mat a in
  let br, bc, b = Slap_mat.opt_mat_alloc prec m n b in
  if __expose_size m <> 0 && __expose_size n <> 0
  then ignore (I.lacpy ?uplo ~m:(__expose_size m) ~n:(__expose_size n)
                 ~br ~bc ~b ~ar ~ac a);
  __unexpose_mat (m, n, br, bc, b)

let of_col_vecs_dyn m n vec_array =
  let convert x =
    assert(Slap_vec.check_cnt x);
    let m', _, _, x = __expose_vec x in
    assert(m = m');
    x
  in
  if __expose_size n <> Array.length vec_array
  then invalid_argf "Mat.of_col_vecs_dyn" ();
  let mat = I.Mat.of_col_vecs (Array.map convert vec_array) in
  __unexpose_mat (m, n, 1, 1, mat)

(** {2 Type conversion} *)

let to_array = Slap_mat.to_array

let of_array_dyn m n array = Slap_mat.of_array_dyn prec m n array

module Of_array (X : sig val value : num_type array array end) : CNTMAT =
  struct
    type m and n
    let value =
      match Slap_mat.dim_array_array X.value with
      | None -> invalid_argf "Mat.Of_array_dyn" ()
      | Some (m, n) ->
        Slap_mat.unsafe_of_array prec
          (__unexpose_size m) (__unexpose_size n) X.value
  end

let of_array aa =
  let module M = Of_array(struct let value = aa end) in
  (module M : CNTMAT)

let to_list = Slap_mat.to_list

let of_list_dyn m n list = Slap_mat.of_list_dyn prec m n list

module Of_list (X : sig val value : num_type list list end) : CNTMAT =
  struct
    type m and n
    let value =
      match Slap_mat.dim_list_list X.value with
      | None -> invalid_argf "Mat.Of_list_dyn" ()
      | Some (m, n) ->
        Slap_mat.unsafe_of_list prec
          (__unexpose_size m) (__unexpose_size n) X.value
  end

let of_list ll =
  let module M = Of_list(struct let value = ll end) in
  (module M : CNTMAT)

(** {2 Iterators} *)

let map f ?b a = Slap_mat.map prec f ?b a

let mapi f ?b a = Slap_mat.mapi prec f ?b a

let fold_left = Slap_mat.fold_left

let fold_lefti = Slap_mat.fold_lefti

let fold_right = Slap_mat.fold_right

let fold_righti = Slap_mat.fold_righti

let fold_top = Slap_mat.fold_top

let fold_topi = Slap_mat.fold_topi

let fold_bottom = Slap_mat.fold_bottom

let fold_bottomi = Slap_mat.fold_bottomi

let replace_all = Slap_mat.replace_all

let replace_alli = Slap_mat.replace_alli

(** {2 Matrix transformations} *)

let transpose_copy a b =
  let m, n, ar, ac, a = __expose_mat a in
  let n', m', br, bc, b = __expose_mat b in
  assert(m = m' && n = n');
  I.Mat.transpose_copy ~m:(__expose_size m) ~n:(__expose_size n)
    ~ar ~ac a ~br ~bc b

let transpose a =
  let m, n, ar, ac, a = __expose_mat a in
  let b = I.Mat.transpose ~m:(__expose_size m) ~n:(__expose_size n) ~ar ~ac a in
  __unexpose_mat (n, m, 1, 1, b)

let detri ?up a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  I.Mat.detri ?up ~n:(__expose_size n) ~ar ~ac a

let packed = Slap_mat.packed

let unpacked ?up ?(fill_num = Some zero) = Slap_mat.unpacked ?up ~fill_num

let geband_dyn = Slap_mat.geband_dyn

let ungeband m kl ku ?(fill_num = Some zero) =
  Slap_mat.ungeband m kl ku ~fill_num

let syband_dyn = Slap_mat.syband_dyn

let unsyband kd ?up ?(fill_num = Some zero) =
  Slap_mat.unsyband kd ?up ~fill_num

let luband_dyn = Slap_mat.luband_dyn

let unluband m kl ku ?(fill_num = Some zero) =
  Slap_mat.unluband m kl ku ~fill_num

(** {2 Arithmetic operations} *)

let wrap1
    (f : ?m:int -> ?n:int -> ?ar:int -> ?ac:int -> I.mat -> _)
    a =
  let m, n, ar, ac, a = __expose_mat a in
  f ~m:(__expose_size m) ~n:(__expose_size n) ~ar ~ac a

let wrap2
    (f : ?m:int -> ?n:int ->
     ?br:int -> ?bc:int -> ?b:I.mat ->
     ?ar:int -> ?ac:int -> I.mat -> _)
    ?b a =
  let m, n, ar, ac, a = __expose_mat a in
  let br, bc, b = Slap_mat.opt_mat_alloc prec m n b in
  ignore (f ~m:(__expose_size m) ~n:(__expose_size n) ~br ~bc ~b ~ar ~ac a);
  __unexpose_mat (m, n, br, bc, b)

let add_const (c:I.num_type) ?b a = wrap2 (I.Mat.add_const c) ?b a

let sum a = wrap1 I.Mat.sum a

let trace a = Vec.sum (diag_rect a)

let scal alpha a =
  wrap1 (fun ?m ?n ?ar ?ac a -> I.Mat.scal alpha ?m ?n ?ar ?ac a) a

let scal_cols a x =
  let m, n', ar, ac, a = __expose_mat a in
  let n, ofsx, incx, x = __expose_vec x in
  assert(n = n' && incx = 1);
  I.Mat.scal_cols ~m:(__expose_size m) ~n:(__expose_size n) ~ar ~ac a ~ofs:ofsx x

let scal_rows x a =
  let m, ofsx, incx, x = __expose_vec x in
  let m', n, ar, ac, a = __expose_mat a in
  assert(m = m' && incx = 1);
  I.Mat.scal_rows ~m:(__expose_size m) ~n:(__expose_size n) ~ofs:ofsx x ~ar ~ac a

let axpy ?alpha ~x y =
  let m, n, xr, xc, x = __expose_mat x in
  let m', n', yr, yc, y = __expose_mat y in
  assert(m = m' && n = n');
  I.Mat.axpy ~m:(__expose_size m) ~n:(__expose_size n) ?alpha ~xr ~xc ~x ~yr ~yc y

let gemm_diag ?beta ?y ~transa ?alpha a ~transb b =
  let an, ak, ar, ac, a = __expose_mat a in
  let bk, bn, br, bc, b = __expose_mat b in
  let n, k = Slap_common.get_transposed_dim transa an ak in
  assert((k, n) = Slap_common.get_transposed_dim transb bk bn);
  let y = Slap_vec.opt_cnt_vec_alloc prec n y in
  ignore (I.Mat.gemm_diag ~n:(__expose_size n) ~k:(__expose_size k) ?beta ~y
                          ~transa:(lacaml_trans3 transa) ?alpha ~ar ~ac a
                          ~transb:(lacaml_trans3 transb) ~br ~bc b);
  __unexpose_vec (n, 1, 1, y)

let syrk_diag ?beta ?y ~trans ?alpha a =
  let an, ak, ar, ac, a = __expose_mat a in
  let n, k = Slap_common.get_transposed_dim trans an ak in
  let y = Slap_vec.opt_cnt_vec_alloc prec n y in
  ignore (I.Mat.syrk_diag ~n:(__expose_size n) ~k:(__expose_size k) ?beta ~y
                          ~trans:(Slap_common.lacaml_trans2 trans)
                          ?alpha ~ar ~ac a);
  __unexpose_vec (n, 1, 1, y)

let gemm_trace ~transa a ~transb b =
  let an, ak, ar, ac, a = __expose_mat a in
  let bk, bn, br, bc, b = __expose_mat b in
  let n, k = Slap_common.get_transposed_dim transa an ak in
  assert((k, n) = Slap_common.get_transposed_dim transb bk bn);
  I.Mat.gemm_trace ~n:(__expose_size n) ~k:(__expose_size k)
                   ~transa:(lacaml_trans3 transa) ~ar ~ac a
                   ~transb:(lacaml_trans3 transb) ~br ~bc b

let syrk_trace a = wrap1 (fun ?m ?n -> I.Mat.syrk_trace ?n:m ?k:n) a

let symm2_trace ?upa a ?upb b =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', n''', br, bc, b = __expose_mat b in
  assert(n = n' && n = n'' && n = n''');
  I.Mat.symm2_trace ~n:(__expose_size n) ?upa ~ar ~ac a ?upb ~br ~bc b

(** {2 Submatrices} *)

let submat_dyn = Slap_mat.submat_dyn
