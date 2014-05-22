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

module F (I    : Slap_module_info.SDCZ)
         (SDCZ : Slap_lacaml.SDCZ with
            type num_type = I.num_type and
            type prec = I.prec and
            type trans3 = I.lacaml_trans) =
struct
  (* interface: slap_SDCZ_mat.ml *)

  module Vec = Slap_vec_impl
  module Mat = Slap_mat_impl
  module Utils = Slap_utils_impl

  include Slap_SDCZ_types_wrap.F(I)

  module type MAT =
    sig
      type m
      type n
      val value : (m, n, 'cnt) mat
    end

  (** {2 Creation of matrices} *)

  let empty = (0, 0, 1, 1, SDCZ.Mat.empty)

  let create m n = Mat.create I.kind m n

  let make m n x = Mat.make I.kind m n x

  let zeros m n = make m n I.zero

  let ones m n = make m n I.one

  let make0 = zeros

  let identity n = (n, n, 1, 1, SDCZ.Mat.identity n)

  let init_cols m n f = Mat.init_cols I.kind m n f

  let init_rows m n f = Mat.init_rows I.kind m n f

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

  let copy_row_dyn mat i =
    let n, ofsx, incx, x = row_dyn mat i in
    ignore (SDCZ.copy ~n ~ofsx ~incx x);
    (n, ofsx, incx, x)

  let diag = Mat.diag

  let copy_diag mat =
    let n, ofsx, incx, x = diag mat in
    ignore (SDCZ.copy ~n ~ofsx ~incx x);
    (n, ofsx, incx, x)

  let as_vec = Mat.as_vec

  (** {2 Basic operations} *)

  let copy ?uplo ?b (m, n, ar, ac, a) =
    let br, bc, b = default_mat m n b in
    if m <> 0 && n <> 0
    then ignore (SDCZ.lacpy ?uplo ~m ~n ~br ~bc ~b ~ar ~ac a);
    (m, n, br, bc, b)

  let of_col_vecs_dyn m n vec_array =
    let convert (m', ofsx, incx, x) =
      assert(m = m');
      if not (Slap_vec_impl.check_cnt m' ofsx incx x) then
        invalid_arg "Mat.of_col_vecs_dyn";
      x
    in
    if n <> Array.length vec_array then invalid_arg "Mat.of_col_vecs_dyn";
    let mat = SDCZ.Mat.of_col_vecs (Array.map convert vec_array) in
    (m, n, 1, 1, mat)

  (** {2 Type conversion} *)

  let to_array = Mat.to_array

  let of_array_dyn m n array = Mat.of_array_dyn I.kind m n array

  module Of_array_dyn (X : sig val value : num_type array array end) : MAT =
    struct
      type m and n
      let value =
        match Utils.dim_array_array X.value with
        | None -> invalid_arg "Mat.Of_array_dyn"
        | Some (m, n) -> Mat.unsafe_of_array I.kind m n X.value
    end

  let to_list = Mat.to_list

  let of_list_dyn m n list = Mat.of_list_dyn I.kind m n list

  module Of_list_dyn (X : sig val value : num_type list list end) : MAT =
    struct
      type m and n
      let value =
        match Utils.dim_list_list X.value with
        | None -> invalid_arg "Mat.Of_list_dyn"
        | Some (m, n) -> Mat.unsafe_of_list I.kind m n X.value
    end

  (** {2 Iterators} *)

  let map f ?b (m, n, ar, ac, a) =
    let br, bc, b = default_mat m n b in
    let _ = SDCZ.Mat.map f ~m ~n ~br ~bc ~b ~ar ~ac a in
    (m, n, br, bc, b)

  let replace_all = Mat.replace_all

  let replace_alli = Mat.replace_alli

  (** {2 Arithmetic operations} *)

  let trace a =
    let n, ofsx, incx, x = diag a in
    SDCZ.Vec.sum ~n ~ofsx ~incx x

  let scal alpha (m, n, ar, ac, a) =
    SDCZ.Mat.scal ~m ~n alpha ~ar ~ac a

  let scal_cols (m, n', ar, ac, a) (n, ofsx, incx, x) =
    assert(n = n' && incx = 1);
    SDCZ.Mat.scal_cols ~m ~n ~ar ~ac a ~ofs:ofsx x

  let scal_rows (m, ofsx, incx, x) (m', n, ar, ac, a) =
    assert(m = m' && incx = 1);
    SDCZ.Mat.scal_rows ~m ~n ~ofs:ofsx x ~ar ~ac a

  let axpy ?alpha ~x:(m, n, xr, xc, x) (m', n', yr, yc, y) =
    assert(m = m' && n = n');
    SDCZ.Mat.axpy ~m ~n ?alpha ~xr ~xc ~x ~yr ~yc y

  let syrk_diag ?beta ?y ~trans ?alpha (an, ak, ar, ac, a) =
    let n, k = get_transposed_dim trans an ak in
    let ofsy, incy, y = default_vec n y in
    assert(Slap_vec_impl.check_cnt n ofsy incy y);
    ignore (SDCZ.Mat.syrk_diag ~n ~k ?beta ~y
                               ~trans:(lacaml_trans2_of_trans trans)
                               ?alpha ~ar ~ac a);
    (n, ofsy, incy, y)

  let gemm_trace ~transa (an, ak, ar, ac, a) ~transb (bk, bn, br, bc, b) =
    let n, k = get_transposed_dim transa an ak in
    assert((k, n) = get_transposed_dim transb bk bn);
    SDCZ.Mat.gemm_trace ~n ~k
                        ~transa:(lacaml_trans3_of_trans transa) ~ar ~ac a
                        ~transb:(lacaml_trans3_of_trans transb) ~br ~bc b

  let symm2_trace ?upa (n, n', ar, ac, a) ?upb (n'', n''', br, bc, b) =
    assert(n = n' && n = n'' && n = n''');
    SDCZ.Mat.symm2_trace ~n ?upa ~ar ~ac a ?upb ~br ~bc b
end
