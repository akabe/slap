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
  let am, an, ar, ac, a = M.__expose a in
  let n', ofsx, incx, x = V.__expose x in
  let m, n = Slap_common.get_transposed_dim trans am an in
  assert(n = n');
  let ofsy, incy, y = Slap_vec.opt_vec_alloc prec m y in
  let vy = V.__unexpose (m, ofsy, incy, y) in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.gemv ~m:(S.__expose am) ~n:(S.__expose an)
                 ?beta ~ofsy ~incy ~y ~trans:(lacaml_trans3 trans)
                 ?alpha ~ar ~ac a ~ofsx ~incx x)
  else Slap_vec.fill vy zero;
  vy

let gbmv ~m:am ?beta ?y ~trans ?alpha a kl ku x =
  let gbs, an, ar, ac, a = M.__expose a in
  let n', ofsx, incx, x = V.__expose x in
  let m, n = Slap_common.get_transposed_dim trans am an in
  assert(gbs = Slap_size.geband_dyn am an kl ku && n = n');
  let ofsy, incy, y = Slap_vec.opt_vec_alloc prec m y in
  let vy = V.__unexpose (m, ofsy, incy, y) in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.gbmv ~m:(S.__expose am) ~n:(S.__expose an)
                 ?beta ~ofsy ~incy ~y ~trans:(lacaml_trans3 trans)
                 ?alpha ~ar ~ac a (S.__expose kl) (S.__expose ku)
                 ~ofsx ~incx x)
  else Slap_vec.fill vy zero;
  vy

let symv ?beta ?y ?up ?alpha a x =
  let n, n', ar, ac, a = M.__expose a in
  let n'', ofsx, incx, x = V.__expose x in
  assert(n = n' && n = n'');
  let ofsy, incy, y = Slap_vec.opt_vec_alloc prec n y in
  let vy = V.__unexpose (n, ofsy, incy, y) in
  if Slap_size.nonzero n
  then ignore (I.symv ~n:(S.__expose n) ?up ?beta ~ofsy ~incy ~y
                 ?alpha ~ar ~ac a ~ofsx ~incx x)
  else Slap_vec.fill vy zero;
  vy

let trmv ~trans ?diag ?up a x =
  let n, n', ar, ac, a = M.__expose a in
  let n'', ofsx, incx, x = V.__expose x in
  assert(n = n' && n = n'');
  if S.__expose n <> 0
  then I.trmv ~n:(S.__expose n) ~trans:(lacaml_trans3 trans)
      ?diag ?up ~ar ~ac a ~ofsx ~incx x

let trsv ~trans ?diag ?up a x =
  let n, n', ar, ac, a = M.__expose a in
  let n'', ofsx, incx, x = V.__expose x in
  assert(n = n' && n = n'');
  if S.__expose n <> 0
  then I.trsv ~n:(S.__expose n) ~trans:(lacaml_trans3 trans)
      ?diag ?up ~ar ~ac a ~ofsx ~incx x

let tpmv ~trans ?diag ?up ap x =
  assert(Slap_vec.check_cnt ap);
  let k, ofsap, _, ap = V.__expose ap in
  let n, ofsx, incx, x = V.__expose x in
  assert(k = Slap_size.packed n);
  if S.__expose n <> 0
  then I.tpmv ~n:(S.__expose n) ~trans:(lacaml_trans3 trans)
      ?diag ?up ~ofsap ap ~ofsx ~incx x

let tpsv ~trans ?diag ?up ap x =
  assert(Slap_vec.check_cnt ap);
  let k, ofsap, _, ap = V.__expose ap in
  let n, ofsx, incx, x = V.__expose x in
  assert(k = Slap_size.packed n);
  if S.__expose n <> 0
  then I.tpsv ~n:(S.__expose n) ~trans:(lacaml_trans3 trans)
      ?diag ?up ~ofsap ap ~ofsx ~incx x

(** {3 Level 3} *)

let gemm ?beta ?c ~transa ?alpha a ~transb b =
  let am, ak, ar, ac, a = M.__expose a in
  let bk, bn, br, bc, b = M.__expose b in
  let m, k = Slap_common.get_transposed_dim transa am ak in
  let k', n = Slap_common.get_transposed_dim transb bk bn in
  assert(k = k');
  let cr, cc, c = Slap_mat.opt_mat_alloc prec m n c in
  let mc = M.__unexpose (m, n, cr, cc, c) in
  if S.__expose m <> 0 && S.__expose n <> 0 && S.__expose k <> 0
  then ignore (I.gemm ~m:(S.__expose m) ~n:(S.__expose n) ~k:(S.__expose k)
                 ?beta ~cr ~cc ~c
                 ~transa:(lacaml_trans3 transa) ?alpha ~ar ~ac a
                 ~transb:(lacaml_trans3 transb) ~br ~bc b)
  else Slap_mat.fill mc zero;
  mc

let symm ~side ?up ?beta ?c ?alpha a b =
  let k, k', ar, ac, a = M.__expose a in
  let m, n, br, bc, b = M.__expose b in
  assert(k = k' && check_side_dim k m n side);
  let cr, cc, c = Slap_mat.opt_mat_alloc prec m n c in
  let mc = M.__unexpose (m, n, cr, cc, c) in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.symm ~m:(S.__expose m) ~n:(S.__expose n)
                 ~side:(lacaml_side side)
                 ?up ?beta ~cr ~cc ~c ?alpha ~ar ~ac a ~br ~bc b)
  else Slap_mat.fill mc zero;
  mc

let trmm ~side ?up ~transa ?diag ?alpha ~a b =
  let k, k', ar, ac, a = M.__expose a in
  let m, n, br, bc, b = M.__expose b in
  assert(k = k' && check_side_dim k m n side);
  if Slap_size.nonzero m && Slap_size.nonzero n
  then I.trmm ~m:(S.__expose m) ~n:(S.__expose n)
      ~side:(lacaml_side side) ?up ~transa:(lacaml_trans3 transa)
      ?diag ?alpha ~ar ~ac ~a ~br ~bc b

let trsm ~side ?up ~transa ?diag ?alpha ~a b =
  let k, k', ar, ac, a = M.__expose a in
  let m, n, br, bc, b = M.__expose b in
  assert(k = k' && check_side_dim k m n side);
  if Slap_size.nonzero m && Slap_size.nonzero n
  then I.trsm ~m:(S.__expose m) ~n:(S.__expose n)
      ~side:(lacaml_side side) ?up ~transa:(lacaml_trans3 transa)
      ?diag ?alpha ~ar ~ac ~a ~br ~bc b

let syrk ?up ?beta ?c ~trans ?alpha a =
  let an, ak, ar, ac, a = M.__expose a in
  let n, k = get_transposed_dim trans an ak in
  let cr, cc, c = Slap_mat.opt_mat_alloc prec n n c in
  let mc = M.__unexpose (n, n, cr, cc, c) in
  if Slap_size.nonzero k
  then ignore (I.syrk ~n:(S.__expose n) ~k:(S.__expose k)
                 ?up ?beta ~cr ~cc ~c ~trans:(lacaml_trans2 trans)
                 ?alpha ~ar ~ac a)
  else Slap_mat.fill mc zero;
  mc

let syr2k ?up ?beta ?c ~trans ?alpha a b =
  let am, an, ar, ac, a = M.__expose a in
  let bm, bn, br, bc, b = M.__expose b in
  assert(am = bm && an = bn);
  let n, k = get_transposed_dim trans am an in
  let cr, cc, c = Slap_mat.opt_mat_alloc prec n n c in
  let mc = M.__unexpose (n, n, cr, cc, c) in
  if Slap_size.nonzero k
  then ignore (I.syr2k ~n:(S.__expose n) ~k:(S.__expose k)
                 ?up ?beta ~cr ~cc ~c ~trans:(lacaml_trans2 trans)
                 ?alpha ~ar ~ac a ~br ~bc b)
  else Slap_mat.fill mc zero;
  mc

(** {2 LAPACK interface} *)

(** {3 Auxiliary routines} *)

let lacpy ?uplo ?b a =
  let m, n, ar, ac, a = M.__expose a in
  let br, bc, b = Slap_mat.opt_mat_alloc prec m n b in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.lacpy ?uplo ~m:(S.__expose m) ~n:(S.__expose n)
                 ~br ~bc ~b ~ar ~ac a);
  M.__unexpose (m, n, br, bc, b)

let lassq ?scale ?sumsq x = Vec.wrap1 (I.lassq ?scale ?sumsq) x

type larnv_liseed = Slap_size.four

let larnv ?idist ?iseed ~x () =
  assert(Slap_vec.check_cnt x);
  let n, _, _, x = V.__expose x in
  ignore (I.larnv ?idist ?iseed:(Slap_vec.opt_cnt_vec Slap_size.four iseed)
            ~n:(S.__expose n) ~x ());
  V.__unexpose (n, 1, 1, x)

type ('m, 'a) lange_min_lwork

let lange_min_lwork n norm =
  S.__unexpose (I.lange_min_lwork (S.__expose n) (lacaml_norm4 norm))

let lange ?norm ?work a =
  let m, n, ar, ac, a = M.__expose a in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then I.lange ~m:(S.__expose m) ~n:(S.__expose n)
      ?norm:(lacaml_norm4_opt norm) ?work:(Slap_vec.opt_work work) ~ar ~ac a
  else 0.0

let lauum ?up a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  if Slap_size.nonzero n then I.lauum ?up ~n:(S.__expose n) ~ar ~ac a

(** {3 Linear equations (computational routines)} *)

let getrf ?ipiv a =
  let m, n, ar, ac, a = M.__expose a in
  let k = Slap_size.min m n in
  let ipiv = Slap_vec.opt_cnt_vec_alloc int32 k ipiv in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.getrf ~m:(S.__expose m) ~n:(S.__expose n) ~ipiv ~ar ~ac a);
  V.__unexpose (k, 1, 1, ipiv)

let getrs ?ipiv ~trans a b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n
  then I.getrs ~n:(S.__expose n)
      ?ipiv:(Slap_vec.opt_cnt_vec (Slap_size.min n n) ipiv)
      ~trans:(lacaml_trans3 trans) ~ar ~ac a ~nrhs:(S.__expose nrhs) ~br ~bc b

type 'n getri_min_lwork

let getri_min_lwork n =
  S.__unexpose (I.getri_min_lwork (S.__expose n))

let getri_opt_lwork a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  I.getri_opt_lwork ~n:(S.__expose n) ~ar ~ac a
  |> Slap_size.unsafe_of_int

let getri ?ipiv ?work a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  if Slap_size.nonzero n
  then I.getri ~n:(S.__expose n)
      ?ipiv:(Slap_vec.opt_cnt_vec (Slap_size.min n n) ipiv)
      ?work:(Slap_vec.opt_work work) ~ar ~ac a

type sytrf_min_lwork

let sytrf_min_lwork () = S.__unexpose (I.sytrf_min_lwork ())

let sytrf_opt_lwork ?up a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  I.sytrf_opt_lwork ~n:(S.__expose n) ?up ~ar ~ac a
  |> Slap_size.unsafe_of_int

let sytrf ?up ?ipiv ?work a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  let ipiv = Slap_vec.opt_cnt_vec_alloc int32 n ipiv in
  if Slap_size.nonzero n
  then ignore (I.sytrf ~n:(S.__expose n) ?up ~ipiv
                 ?work:(Slap_vec.opt_work work) ~ar ~ac a);
  V.__unexpose (n, 1, 1, ipiv)

let sytrs ?up ?ipiv a b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n
  then I.sytrs ~n:(S.__expose n) ?up ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
      ~ar ~ac a ~nrhs:(S.__expose nrhs) ~br ~bc b

type 'n sytri_min_lwork

let sytri_min_lwork n = S.__unexpose (I.sytri_min_lwork (S.__expose n))

let sytri ?up ?ipiv ?work a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  if Slap_size.nonzero n
  then I.sytri ~n:(S.__expose n) ?up ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
      ?work:(Slap_vec.opt_work work) ~ar ~ac a

let potrf ?up ?jitter a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  if Slap_size.nonzero n
  then I.potrf ~n:(S.__expose n) ?up ?jitter ~ar ~ac a

let potrs ?up a ?factorize ?jitter b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n
  then I.potrs ~n:(S.__expose n) ?up ~ar ~ac a
      ~nrhs:(S.__expose nrhs) ~br ~bc ?factorize ?jitter b

let potri ?up ?factorize ?jitter a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  if Slap_size.nonzero n
  then I.potri ~n:(S.__expose n) ?up ?factorize ?jitter ~ar ~ac a

let trtrs ?up ~trans ?diag a b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.trtrs ~n:(S.__expose n) ?up ~trans:(lacaml_trans3 trans)
      ?diag ~ar ~ac a ~nrhs:(S.__expose nrhs) ~br ~bc b

let tbtrs ~kd ?up ~trans ?diag ab b =
  let sbsize, n, abr, abc, ab = M.__expose ab in
  let n', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && sbsize = Slap_size.syband_dyn n kd);
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.tbtrs ~n:(S.__expose n) ~kd:(S.__expose kd)
      ?up ~trans:(lacaml_trans3 trans)
      ?diag ~abr ~abc ab ~nrhs:(S.__expose nrhs) ~br ~bc b

let trtri ?up ?diag a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  if Slap_size.nonzero n then I.trtri ~n:(S.__expose n) ?up ?diag ~ar ~ac a

type 'n geqrf_min_lwork

let geqrf_min_lwork ~n =
  S.__unexpose (I.geqrf_min_lwork ~n:(S.__expose n))

let geqrf_opt_lwork a =
  let m, n, ar, ac, a = M.__expose a in
  I.geqrf_opt_lwork ~m:(S.__expose m) ~n:(S.__expose n) ~ar ~ac a
  |> Slap_size.unsafe_of_int

let geqrf ?work ?tau a =
  let m, n, ar, ac, a = M.__expose a in
  let k = Slap_size.min m n in
  let tau = Slap_vec.opt_cnt_vec_alloc prec k tau in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then ignore (I.geqrf ~m:(S.__expose m) ~n:(S.__expose n)
                 ?work:(Slap_vec.opt_work work) ~tau ~ar ~ac a);
  V.__unexpose (k, 1, 1, tau)

(** {3 Linear equations (simple drivers)} *)

let gesv ?ipiv a b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.gesv ~n:(S.__expose n) ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
      ~ar ~ac a ~nrhs:(S.__expose nrhs) ~br ~bc b

let gbsv ?ipiv ab kl ku b =
  let lusize, n, abr, abc, ab = M.__expose ab in
  let n', nrhs, br, bc, b = M.__expose b in
  assert(lusize = Slap_size.luband_dyn n n kl ku && n = n');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.gbsv ~n:(S.__expose n) ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
      ~abr ~abc ab (S.__expose kl) (S.__expose ku)
      ~nrhs:(S.__expose nrhs) ~br ~bc b

let posv ?up a b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.posv ~n:(S.__expose n) ?up ~ar ~ac a
      ~nrhs:(S.__expose nrhs) ~br ~bc b

let ppsv ?up ap b =
  assert(Slap_vec.check_cnt ap);
  let k, ofsap, _, ap = V.__expose ap in
  let n, nrhs, br, bc, b = M.__expose b in
  assert(k = Slap_size.packed n);
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.ppsv ~n:(S.__expose n) ?up ~ofsap ap
      ~nrhs:(S.__expose nrhs) ~br ~bc b

let pbsv ?up ~kd ab b =
  let sbsize, n, abr, abc, ab = M.__expose ab in
  let n', nrhs, br, bc, b = M.__expose b in
  assert(sbsize = Slap_size.syband_dyn n kd && n = n');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.pbsv ~n:(S.__expose n) ?up ~kd:(S.__expose kd) ~abr ~abc ab
      ~nrhs:(S.__expose nrhs) ~br ~bc b

let ptsv d e b =
  assert(Slap_vec.check_cnt d && Slap_vec.check_cnt e);
  let n, ofsd, _, d = V.__expose d in
  let np, ofse, _, e = V.__expose e in
  let n', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && Slap_size.pred_dyn n = np);
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.ptsv ~n:(S.__expose n) ~ofsd d ~ofse e
      ~nrhs:(S.__expose nrhs) ~br ~bc b

let sysv_opt_lwork ?up a b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && n = n'');
  I.sysv_opt_lwork ~n:(S.__expose n) ?up ~ar ~ac a
    ~nrhs:(S.__expose nrhs) ~br ~bc b
  |> Slap_size.unsafe_of_int

let sysv ?up ?ipiv ?work a b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && n = n'');
  I.sysv ~n:(S.__expose n) ?up ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
    ?work:(Slap_vec.opt_work work) ~ar ~ac a ~nrhs:(S.__expose nrhs) ~br ~bc b

let spsv ?up ?ipiv ap b =
  assert(Slap_vec.check_cnt ap);
  let k, ofsap, _, ap = V.__expose ap in
  let n, nrhs, br, bc, b = M.__expose b in
  assert(k = Slap_size.packed n);
  I.spsv ~n:(S.__expose n) ?up ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
    ~ofsap ap ~nrhs:(S.__expose nrhs) ~br ~bc b

(** {3 Least squares (simple drivers)} *)

type ('m, 'n, 'nrhs) gels_min_lwork

let gels_min_lwork ~m ~n ~nrhs =
  S.__unexpose (I.gels_min_lwork
                    ~m:(S.__expose m) ~n:(S.__expose n)
                    ~nrhs:(S.__expose nrhs))

let gels_opt_lwork ~trans a b =
  let am, an, ar, ac, a = M.__expose a in
  let bm, nrhs, br, bc, b = M.__expose b in
  let m, n = get_transposed_dim trans am an in
  assert(m = bm);
  I.gels_opt_lwork ~m:(S.__expose m) ~n:(S.__expose n)
    ~trans:(lacaml_trans2 trans) ~ar ~ac a ~nrhs:(S.__expose nrhs) ~br ~bc b
  |> Slap_size.unsafe_of_int

let gels ?work ~trans a b =
  let am, an, ar, ac, a = M.__expose a in
  let bm, nrhs, br, bc, b = M.__expose b in
  let m, n = get_transposed_dim trans am an in
  assert(m = bm);
  I.gels ~m:(S.__expose m) ~n:(S.__expose n) ?work:(Slap_vec.opt_work work)
    ~trans:(lacaml_trans2 trans) ~ar ~ac a
    ~nrhs:(S.__expose nrhs) ~br ~bc b
