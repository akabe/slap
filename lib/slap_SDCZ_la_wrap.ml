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
  (* interface: slap_SDCZ_la.ml *)

  include Slap_SDCZ_types_wrap.F(I)

  let kind = I.kind

  let real_kind = I.real_kind

  let prec = I.kind

  let pp_num = SDCZ.pp_num

  let pp_vec fmt (n, ofsx, incx, x) =
    assert(Slap_vec_impl.check_cnt n ofsx incx x);
    SDCZ.pp_vec fmt x

  let pp_mat fmt (m, n, ar, ac, a) =
    assert(Slap_mat_impl.check_cnt m n ar ac a);
    SDCZ.pp_mat fmt a

  let get_transposed_dim trans m n =
    match trans with
    | `N -> (m, n)
    | _  -> (n, m)

  (** {2 BLAS interface} *)

  (** {3 Level 1} *)

  let swap ~x:(n, ofsx, incx, x) (n', ofsy, incy, y) =
    assert(n = n');
    SDCZ.swap ~n ~ofsx ~incx ~x ~ofsy ~incy y

  let scal alpha (n, ofsx, incx, x) =
    SDCZ.scal ~n alpha ~ofsx ~incx x

  let copy ?y (n, ofsx, incx, x) =
    let ofsy, incy, y = default_vec n y in
    let _ = SDCZ.copy ~n ~ofsy ~incy ~y ~ofsx ~incx x in
    (n, ofsy, incy, y)

  let nrm2 (n, ofsx, incx, x) =
    SDCZ.nrm2 ~n ~ofsx ~incx x

  let axpy ?alpha ~x:(n, ofsx, incx, x) (n', ofsy, incy, y) =
    assert(n = n');
    SDCZ.axpy ~n ?alpha ~ofsx ~incx ~x ~ofsy ~incy y

  let iamax (n, ofsx, incx, x) =
    SDCZ.iamax ~n ~ofsx ~incx x

  let amax (n, ofsx, incx, x) =
    SDCZ.amax ~n ~ofsx ~incx x

  (** {3 Level 2} *)

  let gemv ?beta ?y ~trans ?alpha (am, an, ar, ac, a) (n', ofsx, incx, x) =
    let m, n = get_transposed_dim trans am an in
    assert(n = n');
    let ofsy, incy, y = default_vec m y in
    if m <> 0 && n <> 0
    then ignore (SDCZ.gemv ~m:am ~n:an ?beta ~ofsy ~incy ~y
                           ~trans:(I.lacaml_trans_of_trans trans)
                           ?alpha ~ar ~ac a ~ofsx ~incx x)
    else Slap_vec_impl.fill (m, ofsy, incy, y) I.zero;
    (m, ofsy, incy, y)

  let symv ?beta ?y ?up ?alpha (n, n', ar, ac, a) (n'', ofsx, incx, x) =
    assert(n = n' && n = n'');
    let ofsy, incy, y = default_vec n y in
    if n <> 0
    then ignore (SDCZ.symv ~n ?beta ~ofsy ~incy ~y
                           ?alpha ~ar ~ac a ~ofsx ~incx x)
    else Slap_vec_impl.fill (n, ofsy, incy, y) I.zero;
    (n, ofsy, incy, y)

  (** {3 Level 3} *)

  let gemm ?beta ?c ~transa ?alpha (am, ak, ar, ac, a)
           ~transb (bk, bn, br, bc, b) =
    let m, k = get_transposed_dim transa am ak in
    let k', n = get_transposed_dim transb bk bn in
    assert(k = k');
    let cr, cc, c = default_mat m n c in
    if m <> 0 && n <> 0 && k <> 0
    then ignore (SDCZ.gemm ~m ~n ~k ?beta ~cr ~cc ~c
                           ~transa:(I.lacaml_trans_of_trans transa)
                           ?alpha ~ar ~ac a
                           ~transb:(I.lacaml_trans_of_trans transb)
                           ~br ~bc b)
    else Slap_mat_impl.fill (m, n, cr, cc, c) I.zero;
    (m, n, cr, cc, c)

  let symm ~side ?up ?beta ?c ?alpha (k, k', ar, ac, a) (m, n, br, bc, b) =
    assert(k = k' && (if side = `L then k = m else k = n));
    let cr, cc, c = default_mat m n c in
    if m <> 0 && n <> 0
    then ignore (SDCZ.symm ~m ~n ~side ?up ?beta ~cr ~cc ~c
                           ?alpha ~ar ~ac a ~br ~bc b)
    else Slap_mat_impl.fill (m, n, cr, cc, c) I.zero;
    (m, n, cr, cc, c)

  (** {2 LAPACK interface} *)

  (** {3 Auxiliary routines} *)

  let lacpy ?uplo ?b (m, n, ar, ac, a) =
    let br, bc, b = default_mat m n b in
    ignore (SDCZ.lacpy ?uplo ~m ~n ~br ~bc ~b ~ar ~ac a);
    (m, n, br, bc, b)

  let lange ?norm (m, n, ar, ac, a) =
    SDCZ.lange ~m ~n ?norm ~ar ~ac a

  (** {3 Linear equations (computational routines)} *)

  let getrf ?ipiv (m, n, ar, ac, a) =
    let k = min m n in
    let ofs, inc, ipiv = Slap_vec_impl.default_vec Bigarray.int32 k ipiv in
    assert(ofs = 1 && inc = 1); (* ipiv must be a cntvector. *)
    ignore(SDCZ.getrf ~m ~n ~ipiv ~ar ~ac a);
    (k, 1, 1, ipiv)

  let geqrf ?tau (m, n, ar, ac, a) =
    let k = min m n in
    let ofs, inc, tau = default_vec k tau in
    assert(ofs = 1 && inc = 1); (* tau must be a cntvector. *)
    ignore(SDCZ.geqrf ~m ~n ~tau ~ar ~ac a);
    (k, 1, 1, tau)
end
