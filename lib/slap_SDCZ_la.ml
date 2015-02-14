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

let pp_vec ppf x = Slap_io.pp_vec_gen ppf pp_num x

let pp_mat ppf a = Slap_io.pp_mat_gen ppf pp_num a

(** {2 BLAS interface} *)

(** {3 Level 1} *)

let swap ~x y =
  Vec.wrap2 (fun ?n ?ofsx ?incx x -> I.swap ?n ?ofsx ?incx ~x) x y

let scal alpha x = Vec.wrap1 (fun ?n -> I.scal alpha ?n) x

let copy ?y x = Vec.wrap2opt I.copy ?y x

let nrm2 x = Vec.wrap1 I.nrm2 x

let axpy ?alpha ~x y =
  Vec.wrap2 (fun ?n ?ofsx ?incx x -> I.axpy ?alpha ?n ?ofsx ?incx ~x) x y

let iamax x = Vec.wrap1 I.iamax x

let amax x = Vec.wrap1 I.amax x

(** {3 Level 2} *)

let gemv ?beta ?y ~trans ?alpha a x =
  let am, an, ar, ac, a = __expose_mat a in
  let n', ofsx, incx, x = __expose_vec x in
  let m, n = Slap_common.get_transposed_dim trans am an in
  assert(n = n');
  let ofsy, incy, y = Slap_vec.opt_vec_alloc prec m y in
  let vy = __unexpose_vec (m, ofsy, incy, y) in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.gemv ~m:(__expose_size am) ~n:(__expose_size an)
                 ?beta ~ofsy ~incy ~y ~trans:(lacaml_trans3 trans)
                 ?alpha ~ar ~ac a ~ofsx ~incx x)
  else Slap_vec.fill vy zero;
  vy

let gbmv ~m:am ?beta ?y ~trans ?alpha a kl ku x =
  let gbs, an, ar, ac, a = __expose_mat a in
  let n', ofsx, incx, x = __expose_vec x in
  let m, n = Slap_common.get_transposed_dim trans am an in
  assert(gbs = Slap_size.geband_dyn am an kl ku && n = n');
  let ofsy, incy, y = Slap_vec.opt_vec_alloc prec m y in
  let vy = __unexpose_vec (m, ofsy, incy, y) in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.gbmv ~m:(__expose_size am) ~n:(__expose_size an)
                 ?beta ~ofsy ~incy ~y ~trans:(lacaml_trans3 trans)
                 ?alpha ~ar ~ac a (__expose_size kl) (__expose_size ku)
                 ~ofsx ~incx x)
  else Slap_vec.fill vy zero;
  vy

let symv ?beta ?y ?up ?alpha a x =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', ofsx, incx, x = __expose_vec x in
  assert(n = n' && n = n'');
  let ofsy, incy, y = Slap_vec.opt_vec_alloc prec n y in
  let vy = __unexpose_vec (n, ofsy, incy, y) in
  if Slap_size.nonzero n
  then ignore (I.symv ~n:(__expose_size n) ?up ?beta ~ofsy ~incy ~y
                 ?alpha ~ar ~ac a ~ofsx ~incx x)
  else Slap_vec.fill vy zero;
  vy

let trmv ~trans ?diag ?up a x =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', ofsx, incx, x = __expose_vec x in
  assert(n = n' && n = n'');
  if __expose_size n <> 0
  then I.trmv ~n:(__expose_size n) ~trans:(lacaml_trans3 trans)
      ?diag ?up ~ar ~ac a ~ofsx ~incx x

let trsv ~trans ?diag ?up a x =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', ofsx, incx, x = __expose_vec x in
  assert(n = n' && n = n'');
  if __expose_size n <> 0
  then I.trsv ~n:(__expose_size n) ~trans:(lacaml_trans3 trans)
      ?diag ?up ~ar ~ac a ~ofsx ~incx x

let tpmv ~trans ?diag ?up ap x =
  assert(Slap_vec.check_cnt ap);
  let k, ofsap, _, ap = __expose_vec ap in
  let n, ofsx, incx, x = __expose_vec x in
  assert(k = Slap_size.packed n);
  if __expose_size n <> 0
  then I.tpmv ~n:(__expose_size n) ~trans:(lacaml_trans3 trans)
      ?diag ?up ~ofsap ap ~ofsx ~incx x

let tpsv ~trans ?diag ?up ap x =
  assert(Slap_vec.check_cnt ap);
  let k, ofsap, _, ap = __expose_vec ap in
  let n, ofsx, incx, x = __expose_vec x in
  assert(k = Slap_size.packed n);
  if __expose_size n <> 0
  then I.tpsv ~n:(__expose_size n) ~trans:(lacaml_trans3 trans)
      ?diag ?up ~ofsap ap ~ofsx ~incx x

(** {3 Level 3} *)

let gemm ?beta ?c ~transa ?alpha a ~transb b =
  let am, ak, ar, ac, a = __expose_mat a in
  let bk, bn, br, bc, b = __expose_mat b in
  let m, k = Slap_common.get_transposed_dim transa am ak in
  let k', n = Slap_common.get_transposed_dim transb bk bn in
  assert(k = k');
  let cr, cc, c = Slap_mat.opt_mat_alloc prec m n c in
  let mc = __unexpose_mat (m, n, cr, cc, c) in
  if __expose_size m <> 0 && __expose_size n <> 0 && __expose_size k <> 0
  then ignore (I.gemm ~m:(__expose_size m) ~n:(__expose_size n) ~k:(__expose_size k)
                 ?beta ~cr ~cc ~c
                 ~transa:(lacaml_trans3 transa) ?alpha ~ar ~ac a
                 ~transb:(lacaml_trans3 transb) ~br ~bc b)
  else Slap_mat.fill mc zero;
  mc

let symm ~side ?up ?beta ?c ?alpha a b =
  let k, k', ar, ac, a = __expose_mat a in
  let m, n, br, bc, b = __expose_mat b in
  assert(k = k' && check_side_dim k m n side);
  let cr, cc, c = Slap_mat.opt_mat_alloc prec m n c in
  let mc = __unexpose_mat (m, n, cr, cc, c) in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.symm ~m:(__expose_size m) ~n:(__expose_size n)
                 ~side:(lacaml_side side)
                 ?up ?beta ~cr ~cc ~c ?alpha ~ar ~ac a ~br ~bc b)
  else Slap_mat.fill mc zero;
  mc

let trmm ~side ?up ~transa ?diag ?alpha ~a b =
  let k, k', ar, ac, a = __expose_mat a in
  let m, n, br, bc, b = __expose_mat b in
  assert(k = k' && check_side_dim k m n side);
  if Slap_size.nonzero m && Slap_size.nonzero n
  then I.trmm ~m:(__expose_size m) ~n:(__expose_size n)
      ~side:(lacaml_side side) ?up ~transa:(lacaml_trans3 transa)
      ?diag ?alpha ~ar ~ac ~a ~br ~bc b

let trsm ~side ?up ~transa ?diag ?alpha ~a b =
  let k, k', ar, ac, a = __expose_mat a in
  let m, n, br, bc, b = __expose_mat b in
  assert(k = k' && check_side_dim k m n side);
  if Slap_size.nonzero m && Slap_size.nonzero n
  then I.trsm ~m:(__expose_size m) ~n:(__expose_size n)
      ~side:(lacaml_side side) ?up ~transa:(lacaml_trans3 transa)
      ?diag ?alpha ~ar ~ac ~a ~br ~bc b

let syrk ?up ?beta ?c ~trans ?alpha a =
  let an, ak, ar, ac, a = __expose_mat a in
  let n, k = get_transposed_dim trans an ak in
  let cr, cc, c = Slap_mat.opt_mat_alloc prec n n c in
  let mc = __unexpose_mat (n, n, cr, cc, c) in
  if Slap_size.nonzero k
  then ignore (I.syrk ~n:(__expose_size n) ~k:(__expose_size k)
                 ?up ?beta ~cr ~cc ~c ~trans:(lacaml_trans2 trans)
                 ?alpha ~ar ~ac a)
  else Slap_mat.fill mc zero;
  mc

let syr2k ?up ?beta ?c ~trans ?alpha a b =
  let am, an, ar, ac, a = __expose_mat a in
  let bm, bn, br, bc, b = __expose_mat b in
  assert(am = bm && an = bn);
  let n, k = get_transposed_dim trans am an in
  let cr, cc, c = Slap_mat.opt_mat_alloc prec n n c in
  let mc = __unexpose_mat (n, n, cr, cc, c) in
  if Slap_size.nonzero k
  then ignore (I.syr2k ~n:(__expose_size n) ~k:(__expose_size k)
                 ?up ?beta ~cr ~cc ~c ~trans:(lacaml_trans2 trans)
                 ?alpha ~ar ~ac a ~br ~bc b)
  else Slap_mat.fill mc zero;
  mc

(** {2 LAPACK interface} *)

(** {3 Auxiliary routines} *)

let lacpy ?uplo ?b a =
  let m, n, ar, ac, a = __expose_mat a in
  let br, bc, b = Slap_mat.opt_mat_alloc prec m n b in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.lacpy ?uplo ~m:(__expose_size m) ~n:(__expose_size n)
                 ~br ~bc ~b ~ar ~ac a);
  __unexpose_mat (m, n, br, bc, b)

let lassq ?scale ?sumsq x = Vec.wrap1 (I.lassq ?scale ?sumsq) x

type larnv_liseed = Slap_size.four

let larnv ?idist ?iseed ~x () =
  assert(Slap_vec.check_cnt x);
  let n, _, _, x = __expose_vec x in
  ignore (I.larnv ?idist ?iseed:(Slap_vec.opt_cnt_vec Slap_size.four iseed)
            ~n:(__expose_size n) ~x ());
  __unexpose_vec (n, 1, 1, x)

type ('m, 'a) lange_min_lwork

let lange_min_lwork n norm =
  __unexpose_size (I.lange_min_lwork (__expose_size n) (lacaml_norm4 norm))

let lange ?norm ?work a =
  let m, n, ar, ac, a = __expose_mat a in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then I.lange ~m:(__expose_size m) ~n:(__expose_size n)
      ?norm:(lacaml_norm4_opt norm) ?work:(Slap_vec.opt_work work) ~ar ~ac a
  else 0.0

let lauum ?up a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  if Slap_size.nonzero n then I.lauum ?up ~n:(__expose_size n) ~ar ~ac a

(** {3 Linear equations (computational routines)} *)

let getrf ?ipiv a =
  let m, n, ar, ac, a = __expose_mat a in
  let k = Slap_size.min m n in
  let ipiv = Slap_vec.opt_cnt_vec_alloc int32 k ipiv in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.getrf ~m:(__expose_size m) ~n:(__expose_size n) ~ipiv ~ar ~ac a);
  __unexpose_vec (k, 1, 1, ipiv)

let getrs ?ipiv ~trans a b =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', nrhs, br, bc, b = __expose_mat b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n
  then I.getrs ~n:(__expose_size n)
      ?ipiv:(Slap_vec.opt_cnt_vec (Slap_size.min n n) ipiv)
      ~trans:(lacaml_trans3 trans) ~ar ~ac a ~nrhs:(__expose_size nrhs) ~br ~bc b

type 'n getri_min_lwork

let getri_min_lwork n =
  __unexpose_size (I.getri_min_lwork (__expose_size n))

let getri_opt_lwork a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  I.getri_opt_lwork ~n:(__expose_size n) ~ar ~ac a
  |> Slap_size.unsafe_of_int

let getri ?ipiv ?work a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  if Slap_size.nonzero n
  then I.getri ~n:(__expose_size n)
      ?ipiv:(Slap_vec.opt_cnt_vec (Slap_size.min n n) ipiv)
      ?work:(Slap_vec.opt_work work) ~ar ~ac a

type sytrf_min_lwork

let sytrf_min_lwork () = __unexpose_size (I.sytrf_min_lwork ())

let sytrf_opt_lwork ?up a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  I.sytrf_opt_lwork ~n:(__expose_size n) ?up ~ar ~ac a
  |> Slap_size.unsafe_of_int

let sytrf ?up ?ipiv ?work a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  let ipiv = Slap_vec.opt_cnt_vec_alloc int32 n ipiv in
  if Slap_size.nonzero n
  then ignore (I.sytrf ~n:(__expose_size n) ?up ~ipiv
                 ?work:(Slap_vec.opt_work work) ~ar ~ac a);
  __unexpose_vec (n, 1, 1, ipiv)

let sytrs ?up ?ipiv a b =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', nrhs, br, bc, b = __expose_mat b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n
  then I.sytrs ~n:(__expose_size n) ?up ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
      ~ar ~ac a ~nrhs:(__expose_size nrhs) ~br ~bc b

type 'n sytri_min_lwork

let sytri_min_lwork n = __unexpose_size (I.sytri_min_lwork (__expose_size n))

let sytri ?up ?ipiv ?work a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  if Slap_size.nonzero n
  then I.sytri ~n:(__expose_size n) ?up ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
      ?work:(Slap_vec.opt_work work) ~ar ~ac a

let potrf ?up ?jitter a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  if Slap_size.nonzero n
  then I.potrf ~n:(__expose_size n) ?up ?jitter ~ar ~ac a

let potrs ?up a ?factorize ?jitter b =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', nrhs, br, bc, b = __expose_mat b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n
  then I.potrs ~n:(__expose_size n) ?up ~ar ~ac a
      ~nrhs:(__expose_size nrhs) ~br ~bc ?factorize ?jitter b

let potri ?up ?factorize ?jitter a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  if Slap_size.nonzero n
  then I.potri ~n:(__expose_size n) ?up ?factorize ?jitter ~ar ~ac a

let trtrs ?up ~trans ?diag a b =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', nrhs, br, bc, b = __expose_mat b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.trtrs ~n:(__expose_size n) ?up ~trans:(lacaml_trans3 trans)
      ?diag ~ar ~ac a ~nrhs:(__expose_size nrhs) ~br ~bc b

let tbtrs ~kd ?up ~trans ?diag ab b =
  let sbsize, n, abr, abc, ab = __expose_mat ab in
  let n', nrhs, br, bc, b = __expose_mat b in
  assert(n = n' && sbsize = Slap_size.syband_dyn n kd);
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.tbtrs ~n:(__expose_size n) ~kd:(__expose_size kd)
      ?up ~trans:(lacaml_trans3 trans)
      ?diag ~abr ~abc ab ~nrhs:(__expose_size nrhs) ~br ~bc b

let trtri ?up ?diag a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  if Slap_size.nonzero n then I.trtri ~n:(__expose_size n) ?up ?diag ~ar ~ac a

type 'n geqrf_min_lwork

let geqrf_min_lwork ~n =
  __unexpose_size (I.geqrf_min_lwork ~n:(__expose_size n))

let geqrf_opt_lwork a =
  let m, n, ar, ac, a = __expose_mat a in
  I.geqrf_opt_lwork ~m:(__expose_size m) ~n:(__expose_size n) ~ar ~ac a
  |> Slap_size.unsafe_of_int

let geqrf ?work ?tau a =
  let m, n, ar, ac, a = __expose_mat a in
  let k = Slap_size.min m n in
  let tau = Slap_vec.opt_cnt_vec_alloc prec k tau in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.geqrf ~m:(__expose_size m) ~n:(__expose_size n)
                 ?work:(Slap_vec.opt_work work) ~tau ~ar ~ac a);
  __unexpose_vec (k, 1, 1, tau)

(** {3 Linear equations (simple drivers)} *)

let gesv ?ipiv a b =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', nrhs, br, bc, b = __expose_mat b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.gesv ~n:(__expose_size n) ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
      ~ar ~ac a ~nrhs:(__expose_size nrhs) ~br ~bc b

let gbsv ?ipiv ab kl ku b =
  let lusize, n, abr, abc, ab = __expose_mat ab in
  let n', nrhs, br, bc, b = __expose_mat b in
  assert(lusize = Slap_size.luband_dyn n n kl ku && n = n');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.gbsv ~n:(__expose_size n) ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
      ~abr ~abc ab (__expose_size kl) (__expose_size ku)
      ~nrhs:(__expose_size nrhs) ~br ~bc b

let posv ?up a b =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', nrhs, br, bc, b = __expose_mat b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.posv ~n:(__expose_size n) ?up ~ar ~ac a
      ~nrhs:(__expose_size nrhs) ~br ~bc b

let ppsv ?up ap b =
  assert(Slap_vec.check_cnt ap);
  let k, ofsap, _, ap = __expose_vec ap in
  let n, nrhs, br, bc, b = __expose_mat b in
  assert(k = Slap_size.packed n);
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.ppsv ~n:(__expose_size n) ?up ~ofsap ap
      ~nrhs:(__expose_size nrhs) ~br ~bc b

let pbsv ?up ~kd ab b =
  let sbsize, n, abr, abc, ab = __expose_mat ab in
  let n', nrhs, br, bc, b = __expose_mat b in
  assert(sbsize = Slap_size.syband_dyn n kd && n = n');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.pbsv ~n:(__expose_size n) ?up ~kd:(__expose_size kd) ~abr ~abc ab
      ~nrhs:(__expose_size nrhs) ~br ~bc b

let ptsv d e b =
  assert(Slap_vec.check_cnt d && Slap_vec.check_cnt e);
  let n, ofsd, _, d = __expose_vec d in
  let np, ofse, _, e = __expose_vec e in
  let n', nrhs, br, bc, b = __expose_mat b in
  assert(n = n' && Slap_size.pred_dyn n = np);
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.ptsv ~n:(__expose_size n) ~ofsd d ~ofse e
      ~nrhs:(__expose_size nrhs) ~br ~bc b

let sysv_opt_lwork ?up a b =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', nrhs, br, bc, b = __expose_mat b in
  assert(n = n' && n = n'');
  I.sysv_opt_lwork ~n:(__expose_size n) ?up ~ar ~ac a
    ~nrhs:(__expose_size nrhs) ~br ~bc b
  |> Slap_size.unsafe_of_int

let sysv ?up ?ipiv ?work a b =
  let n, n', ar, ac, a = __expose_mat a in
  let n'', nrhs, br, bc, b = __expose_mat b in
  assert(n = n' && n = n'');
  I.sysv ~n:(__expose_size n) ?up ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
    ?work:(Slap_vec.opt_work work) ~ar ~ac a ~nrhs:(__expose_size nrhs) ~br ~bc b

let spsv ?up ?ipiv ap b =
  assert(Slap_vec.check_cnt ap);
  let k, ofsap, _, ap = __expose_vec ap in
  let n, nrhs, br, bc, b = __expose_mat b in
  assert(k = Slap_size.packed n);
  I.spsv ~n:(__expose_size n) ?up ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
    ~ofsap ap ~nrhs:(__expose_size nrhs) ~br ~bc b

(** {3 Least squares (simple drivers)} *)

type ('m, 'n, 'nrhs) gels_min_lwork

let gels_min_lwork ~m ~n ~nrhs =
  __unexpose_size (I.gels_min_lwork
                    ~m:(__expose_size m) ~n:(__expose_size n)
                    ~nrhs:(__expose_size nrhs))

let gels_opt_lwork ~trans a b =
  let am, an, ar, ac, a = __expose_mat a in
  let bm, nrhs, br, bc, b = __expose_mat b in
  let m, n = get_transposed_dim trans am an in
  assert(m = bm);
  I.gels_opt_lwork ~m:(__expose_size m) ~n:(__expose_size n)
    ~trans:(lacaml_trans2 trans) ~ar ~ac a ~nrhs:(__expose_size nrhs) ~br ~bc b
  |> Slap_size.unsafe_of_int

let gels ?work ~trans a b =
  let am, an, ar, ac, a = __expose_mat a in
  let bm, nrhs, br, bc, b = __expose_mat b in
  let m, n = get_transposed_dim trans am an in
  assert(m = bm);
  I.gels ~m:(__expose_size m) ~n:(__expose_size n) ?work:(Slap_vec.opt_work work)
    ~trans:(lacaml_trans2 trans) ~ar ~ac a
    ~nrhs:(__expose_size nrhs) ~br ~bc b
