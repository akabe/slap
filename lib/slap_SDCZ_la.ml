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

let pp_num = I.pp_num

let pp_vec ppf x = Io.pp_vec_gen ppf pp_num x

let pp_mat ppf a = Io.pp_mat_gen ppf pp_num a

(** {2 BLAS interface} *)

(** {3 Level 1} *)

let swap ~x:(n, ofsx, incx, x) (n', ofsy, incy, y) =
  assert(n = n');
  I.swap ~n ~ofsx ~incx ~x ~ofsy ~incy y

let scal alpha (n, ofsx, incx, x) =
  I.scal ~n alpha ~ofsx ~incx x

let copy ?y (n, ofsx, incx, x) =
  let ofsy, incy, y = PVec.opt_vec_alloc prec n y in
  let _ = I.copy ~n ~ofsy ~incy ~y ~ofsx ~incx x in
  (n, ofsy, incy, y)

let nrm2 (n, ofsx, incx, x) =
  I.nrm2 ~n ~ofsx ~incx x

let axpy ?alpha ~x:(n, ofsx, incx, x) (n', ofsy, incy, y) =
  assert(n = n');
  I.axpy ~n ?alpha ~ofsx ~incx ~x ~ofsy ~incy y

let iamax (n, ofsx, incx, x) =
  I.iamax ~n ~ofsx ~incx x

let amax (n, ofsx, incx, x) =
  I.amax ~n ~ofsx ~incx x

(** {3 Level 2} *)

let gemv ?beta ?y ~trans ?alpha (am, an, ar, ac, a) (n', ofsx, incx, x) =
  let m, n = Common.get_transposed_dim trans am an in
  assert(n = n');
  let ofsy, incy, y = PVec.opt_vec_alloc prec m y in
  if m <> 0 && n <> 0
  then ignore (I.gemv ~m:am ~n:an ?beta ~ofsy ~incy ~y
                      ~trans:(lacaml_trans3 trans)
                      ?alpha ~ar ~ac a ~ofsx ~incx x)
  else PVec.fill (m, ofsy, incy, y) zero;
  (m, ofsy, incy, y)

let symv ?beta ?y ?up ?alpha (n, n', ar, ac, a) (n'', ofsx, incx, x) =
  assert(n = n' && n = n'');
  let ofsy, incy, y = PVec.opt_vec_alloc prec n y in
  if n <> 0
  then ignore (I.symv ~n ?beta ~ofsy ~incy ~y
                      ?alpha ~ar ~ac a ~ofsx ~incx x)
  else PVec.fill (n, ofsy, incy, y) zero;
  (n, ofsy, incy, y)

let trmv ~trans ?diag ?up (n, n', ar, ac, a) (n'', ofsx, incx, x) =
  assert(n = n' && n = n'');
  if n <> 0
  then I.trmv ~n ~trans:(lacaml_trans3 trans)
                 ?diag ?up ~ar ~ac a ~ofsx ~incx x

let trsv ~trans ?diag ?up (n, n', ar, ac, a) (n'', ofsx, incx, x) =
  assert(n = n' && n = n'');
  if n <> 0
  then I.trsv ~n ~trans:(lacaml_trans3 trans)
              ?diag ?up ~ar ~ac a ~ofsx ~incx x

(** {3 Level 3} *)

let gemm ?beta ?c ~transa ?alpha (am, ak, ar, ac, a)
         ~transb (bk, bn, br, bc, b) =
  let m, k = Common.get_transposed_dim transa am ak in
  let k', n = Common.get_transposed_dim transb bk bn in
  assert(k = k');
  let cr, cc, c = PMat.opt_mat_alloc prec m n c in
  if m <> 0 && n <> 0 && k <> 0
  then ignore (I.gemm ~m ~n ~k ?beta ~cr ~cc ~c
                      ~transa:(lacaml_trans3 transa)
                      ?alpha ~ar ~ac a
                      ~transb:(lacaml_trans3 transb)
                      ~br ~bc b)
  else PMat.fill (m, n, cr, cc, c) zero;
  (m, n, cr, cc, c)

let symm ~side ?up ?beta ?c ?alpha (k, k', ar, ac, a) (m, n, br, bc, b) =
  assert(k = k' && (if side = `L then k = m else k = n));
  let cr, cc, c = PMat.opt_mat_alloc prec m n c in
  if m <> 0 && n <> 0
  then ignore (I.symm ~m ~n ~side ?up ?beta ~cr ~cc ~c
                      ?alpha ~ar ~ac a ~br ~bc b)
  else PMat.fill (m, n, cr, cc, c) zero;
  (m, n, cr, cc, c)

let trmm ~side ?up ~transa ?diag ?alpha
         ~a:(k, k', ar, ac, a) (m, n, br, bc, b) =
  assert(k = k' && (if side = `L then k = m else k = n));
  if m <> 0 && n <> 0
  then I.trmm ~m ~n ~side ?up ~transa:(lacaml_trans3 transa)
                 ?diag ?alpha ~ar ~ac ~a ~br ~bc b

let trsm ~side ?up ~transa ?diag ?alpha
         ~a:(k, k', ar, ac, a) (m, n, br, bc, b) =
  assert(k = k' && (if side = `L then k = m else k = n));
  if m <> 0 && n <> 0
  then I.trsm ~m ~n ~side ?up ~transa:(lacaml_trans3 transa)
                 ?diag ?alpha ~ar ~ac ~a ~br ~bc b

let syrk ?up ?beta ?c ~trans ?alpha (an, ak, ar, ac, a) =
  let n, k = Common.get_transposed_dim trans an ak in
  let cr, cc, c = PMat.opt_mat_alloc prec n n c in
  if k <> 0
  then ignore (I.syrk ~n ~k ?up ?beta ~cr ~cc ~c
                      ~trans:(Common.lacaml_trans2 trans)
                      ?alpha ~ar ~ac a)
  else PMat.fill (n, n, cr, cc, c) zero;
  (n, n, cr, cc, c)

let syr2k ?up ?beta ?c ~trans ?alpha (am, an, ar, ac, a) (bm, bn, br, bc, b) =
  assert(am = bm && an = bn);
  let n, k = Common.get_transposed_dim trans am an in
  let cr, cc, c = PMat.opt_mat_alloc prec n n c in
  if k <> 0
  then ignore (I.syr2k ~n ~k ?up ?beta ~cr ~cc ~c
                       ~trans:(Common.lacaml_trans2 trans)
                       ?alpha ~ar ~ac a ~br ~bc b)
  else PMat.fill (n, n, cr, cc, c) zero;
  (n, n, cr, cc, c)

(** {2 LAPACK interface} *)

(** {3 Auxiliary routines} *)

let lacpy ?uplo ?b (m, n, ar, ac, a) =
  let br, bc, b = PMat.opt_mat_alloc prec m n b in
  if m <> 0 && n <> 0
  then ignore (I.lacpy ?uplo ~m ~n ~br ~bc ~b ~ar ~ac a);
  (m, n, br, bc, b)

let lassq ?scale ?sumsq (n, ofsx, incx, x) =
  I.lassq ~n ?scale ?sumsq ~ofsx ~incx x

type larnv_liseed = Size.z Size.s Size.s Size.s Size.s

let larnv ?idist ?iseed ~x:(n, ofsx, incx, x) () =
  assert(PVec.check_cnt n ofsx incx x);
  ignore (I.larnv ?idist ?iseed:(PVec.opt_cnt_vec 4 iseed) ~n ~x ());
  (n, 1, 1, x)

type ('m, 'a) lange_min_lwork

let lange_min_lwork = I.lange_min_lwork

let lange ?norm ?work (m, n, ar, ac, a) =
  if m <> 0 && n <> 0
  then I.lange ~m ~n ?norm ?work:(PVec.opt_work work) ~ar ~ac a
  else 0.0

let lauum ?up (n, n', ar, ac, a) =
  assert(n = n');
  if n <> 0 then I.lauum ?up ~n ~ar ~ac a

(** {3 Linear equations (computational routines)} *)

let getrf ?ipiv (m, n, ar, ac, a) =
  let k = min m n in
  let ipiv = PVec.opt_cnt_vec_alloc int32 k ipiv in
  if m <> 0 && n <> 0 then ignore (I.getrf ~m ~n ~ipiv ~ar ~ac a);
  (k, 1, 1, ipiv)

let getrs ?ipiv ~trans (n, n', ar, ac, a) (n'', nrhs, br, bc, b) =
  assert(n = n' && n = n'');
  if n <> 0 then I.getrs ~n ?ipiv:(PVec.opt_cnt_vec n ipiv)
                         ~trans:(lacaml_trans3 trans)
                         ~ar ~ac a ~nrhs ~br ~bc b

type 'n getri_min_lwork

let getri_min_lwork = I.getri_min_lwork

let getri_opt_lwork (n, n', ar, ac, a) =
  assert(n = n');
  I.getri_opt_lwork ~n ~ar ~ac a
  |> Size.unsafe_of_int

let getri ?ipiv ?work (n, n', ar, ac, a) =
  assert(n = n');
  if n <> 0 then I.getri ~n ?ipiv:(PVec.opt_cnt_vec n ipiv)
                         ?work:(PVec.opt_work work)
                         ~ar ~ac a

type sytrf_min_lwork

let sytrf_min_lwork = I.sytrf_min_lwork

let sytrf_opt_lwork ?up (n, n', ar, ac, a) =
  assert(n = n');
  I.sytrf_opt_lwork ~n ?up ~ar ~ac a
  |> Size.unsafe_of_int

let sytrf ?up ?ipiv ?work (n, n', ar, ac, a) =
  assert(n = n');
  let ipiv = PVec.opt_cnt_vec_alloc int32 n ipiv in
  if n <> 0 then ignore (I.sytrf ~n ?up ~ipiv
                                 ?work:(PVec.opt_work work)
                                 ~ar ~ac a);
  (n, 1, 1, ipiv)

let sytrs ?up ?ipiv (n, n', ar, ac, a) (n'', nrhs, br, bc, b) =
  assert(n = n' && n = n'');
  if n <> 0 then I.sytrs ~n ?up ?ipiv:(PVec.opt_cnt_vec n ipiv)
                         ~ar ~ac a ~nrhs ~br ~bc b

type 'n sytri_min_lwork

let sytri_min_lwork = I.sytri_min_lwork

let sytri ?up ?ipiv ?work (n, n', ar, ac, a) =
  assert(n = n');
  if n <> 0 then I.sytri ~n ?up ?ipiv:(PVec.opt_cnt_vec n ipiv)
                         ?work:(PVec.opt_work work)
                         ~ar ~ac a

let potrf ?up ?jitter (n, n', ar, ac, a) =
  assert(n = n');
  if n <> 0 then I.potrf ~n ?up ?jitter ~ar ~ac a

let potrs ?up (n, n', ar, ac, a) ?factorize ?jitter (n'', nrhs, br, bc, b) =
  assert(n = n' && n = n'');
  if n <> 0 then I.potrs ~n ?up ~ar ~ac a ~nrhs ~br ~bc ?factorize ?jitter b

let potri ?up ?factorize ?jitter (n, n', ar, ac, a) =
  assert(n = n');
  if n <> 0 then I.potri ~n ?up ?factorize ?jitter ~ar ~ac a

let trtrs ?up ~trans ?diag (n, n', ar, ac, a) (n'', nrhs, br, bc, b) =
  assert(n = n' && n = n'');
  if n <> 0 && nrhs <> 0
  then I.trtrs ~n ?up ~trans:(lacaml_trans3 trans)
               ?diag ~ar ~ac a ~nrhs ~br ~bc b

let trtri ?up ?diag (n, n', ar, ac, a) =
  assert(n = n');
  if n <> 0 then I.trtri ~n ?up ?diag ~ar ~ac a

type 'n geqrf_min_lwork

let geqrf_min_lwork = I.geqrf_min_lwork

let geqrf_opt_lwork (m, n, ar, ac, a) =
  I.geqrf_opt_lwork ~m ~n ~ar ~ac a
  |> Size.unsafe_of_int

let geqrf ?work ?tau (m, n, ar, ac, a) =
  let k = min m n in
  let tau = PVec.opt_cnt_vec_alloc prec k tau in
  if m <> 0 && n <> 0
  then ignore (I.geqrf ~m ~n ?work:(PVec.opt_work work)
                       ~tau ~ar ~ac a);
  (k, 1, 1, tau)

(** {3 Linear equations (simple drivers)} *)

let gesv ?ipiv (n, n', ar, ac, a) (n'', nrhs, br, bc, b) =
  assert(n = n' && n = n'');
  if n <> 0 && nrhs <> 0
  then I.gesv ~n ?ipiv:(PVec.opt_cnt_vec n ipiv) ~ar ~ac a ~nrhs ~br ~bc b

let posv ?up (n, n', ar, ac, a) (n'', nrhs, br, bc, b) =
  assert(n = n' && n = n'');
  if n <> 0 && nrhs <> 0
  then I.posv ~n ?up ~ar ~ac a ~nrhs ~br ~bc b

let sysv_opt_lwork ?up (n, n', ar, ac, a) (n'', nrhs, br, bc, b) =
  assert(n = n' && n = n'');
  I.sysv_opt_lwork ~n ?up ~ar ~ac a ~nrhs ~br ~bc b
  |> Size.unsafe_of_int

let sysv ?up ?ipiv ?work (n, n', ar, ac, a) (n'', nrhs, br, bc, b) =
  assert(n = n' && n = n'');
  I.sysv ~n ?up ?ipiv:(PVec.opt_cnt_vec n ipiv)
         ?work:(PVec.opt_work work)
         ~ar ~ac a ~nrhs ~br ~bc b

(** {3 Least squares (simple drivers)} *)

type ('m, 'n, 'nrhs) gels_min_lwork

let gels_min_lwork = I.gels_min_lwork

let gels_opt_lwork ~trans (am, an, ar, ac, a) (bm, nrhs, br, bc, b) =
  assert(am = bm);
  let m, n = Common.get_transposed_dim trans am an in
  I.gels_opt_lwork ~m ~n ~trans:(Common.lacaml_trans2 trans)
                   ~ar ~ac a ~nrhs ~br ~bc b
  |> Size.unsafe_of_int

let gels ?work ~trans (am, an, ar, ac, a) (bm, nrhs, br, bc, b) =
  assert(am = bm);
  let m, n = Common.get_transposed_dim trans am an in
  I.gels ~m ~n ?work:(PVec.opt_work work)
         ~trans:(Common.lacaml_trans2 trans) ~ar ~ac a ~nrhs ~br ~bc b
