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

(** {2 BLAS interface} *)

(** {3 Level 1} *)

let dot ~x:(n, ofsx, incx, x) (n', ofsy, incy, y) =
  assert(n = n');
  I.dot ~n ~ofsx ~incx ~x ~ofsy ~incy y

let asum (n, ofsx, incx, x) =
  I.asum ~n ~ofsx ~incx x

(** {3 Level 2} *)

let sbmv ~k ?y (sbs, n, ar, ac, a) ?up ?alpha ?beta (n', ofsx, incx, x) =
  assert(n = n' && sbs = Size.syband_dyn n k);
  let ofsy, incy, y = PVec.opt_vec_alloc prec n y in
  ignore (I.sbmv ~n ~k ~ofsy ~incy ~y ~ar ~ac a ?up ?alpha ?beta ~ofsx ~incx x);
  (n, ofsy, incy, y)

let ger ?(alpha = 1.0) (m, ofsx, incx, x) (n, ofsy, incy, y)
        (m', n', ar, ac, a) =
  assert(m = m' && n = n');
  ignore (I.ger ~m ~alpha ~ofsx ~incx x ~ofsy ~incy y ~ar ~ac a);
  (m, n, ar, ac, a)

let syr ?(alpha = 1.0) ?(up = true) (n, ofsx, incx, x)
        (n', n'', ar, ac, a) =
  assert(n = n' && n = n'');
  ignore(I.syr ~n ~alpha ~up ~ofsx ~incx x ~ar ~ac a);
  (n, n, ar, ac, a)

(** {2 LAPACK interface} *)

(** {3 Auxiliary routines} *)

(** {4 lansy} *)

type ('m, 'a) lansy_min_lwork

let lansy_min_lwork = I.lansy_min_lwork

let lansy ?up ?norm ?work (n, n', ar, ac, a) =
  assert(n = n');
  I.lansy ~n ?up ?norm ?work:(PVec.opt_work work) ~ar ~ac a

(** {4 lamch} *)

let lamch = I.lamch

(** {3 Linear equations (computational routines)} *)

(** {4 orgqr} *)

let check_orgqr_args loc k ofst inct tau m n =
  assert(PVec.check_cnt k ofst inct tau);
  if m < n || n < k then invalid_arg (loc ^ ": m >= n >= k")

type 'n orgqr_min_lwork

let orgqr_min_lwork = I.orgqr_min_lwork

let orgqr_opt_lwork ~tau:(k, ofst, inct, tau) (m, n, ar, ac, a) =
  check_orgqr_args "orgqr_opt_lwork" k ofst inct tau m n;
  I.orgqr_opt_lwork ~m ~n ~k ~tau ~ar ~ac a
  |> Size.unsafe_of_int

let orgqr_dyn ?work ~tau:(k, ofst, inct, tau) (m, n, ar, ac, a) =
  check_orgqr_args "orgqr_dyn" k ofst inct tau m n;
  I.orgqr ~m ~n ~k ?work:(PVec.opt_work work) ~tau ~ar ~ac a

(** {4 ormqr} *)

let check_ormqr ~loc ~side ~r ~m ~n ~k ~k' ~ofst ~inct ~tau =
  assert(k = k');
  assert(PVec.check_cnt k ofst inct tau);
  match side with
  | `L ->
     assert(r = m);
     if k > m then invalid_arg loc
  | `R ->
     assert(r = n);
     if k > m then invalid_arg loc

type ('r, 'm, 'n) ormqr_min_lwork

let ormqr_min_lwork ~side ~m ~n =
  match side with
  | `L -> max 1 n
  | `R -> max 1 m

let ormqr_opt_lwork ~side ~trans ~tau:(k, ofst, inct, tau)
                    (r, k', ar, ac, a) (m, n, cr, cc, c) =
  check_ormqr ~loc:"ormqr_opt_lwork" ~side ~r ~m ~n ~k ~k' ~ofst ~inct ~tau;
  I.ormqr_opt_lwork ~side
                     ~trans:(Common.lacaml_trans2 trans)
                     ~m ~n ~k ~tau ~ar ~ac a ~cr ~cc c
  |> Size.unsafe_of_int

let ormqr_dyn ~side ~trans ?work ~tau:(k, ofst, inct, tau)
              (r, k', ar, ac, a) (m, n, cr, cc, c) =
  check_ormqr ~loc:"ormqr_dyn" ~side ~r ~m ~n ~k ~k' ~ofst ~inct ~tau;
  I.ormqr ~side ~trans:(Common.lacaml_trans2 trans) ~m ~n ~k
          ?work:(PVec.opt_work work) ~tau ~ar ~ac a ~cr ~cc c

(** {4 gecon} *)

type 'n gecon_min_lwork

let gecon_min_lwork = I.gecon_min_lwork

type 'n gecon_min_liwork

let gecon_min_liwork = I.gecon_min_liwork

let gecon ?norm ?anorm ?work ?iwork (n, n', ar, ac, a) =
  assert(n = n');
  I.gecon ~n ?norm:(Common.lacaml_norm2_opt norm) ?anorm
           ?work:(PVec.opt_work work)
           ?iwork:(PVec.opt_work iwork)
           ~ar ~ac a

(** {4 sycon} *)

type 'n sycon_min_lwork

let sycon_min_lwork = I.sycon_min_lwork

type 'n sycon_min_liwork

let sycon_min_liwork = I.sycon_min_liwork

let sycon ?up ?ipiv ?anorm ?work ?iwork (n, n', ar, ac, a) =
  assert(n = n');
  I.sycon ~n ?up ?ipiv:(PVec.opt_cnt_vec n ipiv) ?anorm
           ?work:(PVec.opt_work work)
           ?iwork:(PVec.opt_work iwork)
           ~ar ~ac a

(** {4 pocon} *)

type 'n pocon_min_lwork

let pocon_min_lwork = I.pocon_min_lwork

type 'n pocon_min_liwork

let pocon_min_liwork = I.pocon_min_liwork

let pocon ?up ?anorm ?work ?iwork (n, n', ar, ac, a) =
  assert(n = n');
  I.pocon ~n ?up ?anorm
           ?work:(PVec.opt_work work)
           ?iwork:(PVec.opt_work iwork)
           ~ar ~ac a

(** {3 Least squares (expert drivers)} *)

(** {4 gelsy} *)

type ('m, 'n, 'nrhs) gelsy_min_lwork

let gelsy_min_lwork = I.gelsy_min_lwork

let gelsy_opt_lwork (m, n, ar, ac, a) (n', nrhs, br, bc, b) =
  assert(n = n');
  I.gelsy_opt_lwork ~m ~n ~ar ~ac a ~nrhs ~br ~bc b
  |> Size.unsafe_of_int

let gelsy (m, n, ar, ac, a) ?rcond ?jpvt ?work (n', nrhs, br, bc, b) =
  assert(n = n');
  I.gelsy ~m ~n ~ar ~ac a ?rcond
           ?jpvt:(PVec.opt_cnt_vec n jpvt)
           ?work:(PVec.opt_work work)
           ~nrhs ~br ~bc b

(** {4 gelsd} *)

type ('m, 'n, 'nrhs) gelsd_min_lwork

let gelsd_min_lwork = I.gelsd_min_lwork

let gelsd_opt_lwork (m, n, ar, ac, a) (n', nrhs, br, bc, b) =
  assert(n = n');
  I.gelsd_opt_lwork ~m ~n ~ar ~ac a ~nrhs ~br ~bc b
  |> Size.unsafe_of_int

type ('m, 'n, 'nrhs) gelsd_min_iwork

let gelsd_min_iwork = I.gelsd_min_iwork

let gelsd (m, n, ar, ac, a) ?rcond ?s ?work ?iwork
          (n', nrhs, br, bc, b) =
  assert(n = n');
  I.gelsd ~m ~n ~ar ~ac a ?rcond
           ?s:(PVec.opt_cnt_vec (min m n) s)
           ?work:(PVec.opt_work work)
           ?iwork:(PVec.opt_work iwork)
           ~nrhs ~br ~bc b

(** {4 gelss} *)

type ('m, 'n, 'nrhs) gelss_min_lwork

let gelss_min_lwork = I.gelss_min_lwork

let gelss_opt_lwork (m, n, ar, ac, a) (n', nrhs, br, bc, b) =
  assert(n = n');
  I.gelss_opt_lwork ~m ~n ~ar ~ac a ~nrhs ~br ~bc b
  |> Size.unsafe_of_int

let gelss (m, n, ar, ac, a) ?rcond ?s ?work (n', nrhs, br, bc, b) =
  assert(n = n');
  I.gelss ~m ~n ~ar ~ac a ?rcond
           ?s:(PVec.opt_cnt_vec (min m n) s)
           ?work:(PVec.opt_work work)
           ~nrhs ~br ~bc b

(** {3 General SVD routines} *)

(** {4 gesvd} *)

let opt_mat m n = function
  | None -> None, None, None
  | Some (m', n', ar, ac, a) ->
     assert(m = m' && n = n');
     Some ar, Some ac, Some a

let opt_mat_alloc = PMat.opt_mat_alloc

type ('m, 'n) gesvd_min_lwork

let gesvd_min_lwork = I.gesvd_min_lwork

let gesvd_calc_sizes m n jobu jobvt =
  let min_mn = min m n in
  let job_size c = function
    | `A -> c
    | `S -> min_mn
    | `O | `N -> 0 in
  let u_cols = job_size m jobu in
  let vt_rows = job_size n jobvt in
  (min_mn, u_cols, vt_rows)

let gesvd_opt_lwork ~jobu ~jobvt ?s ?u ?vt (m, n, ar, ac, a) =
  let min_mn, u_cols, vt_rows = gesvd_calc_sizes m n jobu jobvt in
  let ur, uc, u = opt_mat m u_cols u in
  let vtr, vtc, vt = opt_mat vt_rows n vt in
  I.gesvd_opt_lwork ~m ~n ~jobu ~jobvt ?s:(PVec.opt_cnt_vec min_mn s)
                    ?ur ?uc ?u ?vtr ?vtc ?vt ~ar ~ac a
  |> Size.unsafe_of_int

let gesvd ~jobu ~jobvt ?s ?u ?vt ?work (m, n, ar, ac, a) =
  let min_mn, u_cols, vt_rows = gesvd_calc_sizes m n jobu jobvt in
  let ur, uc, u = opt_mat_alloc prec m u_cols u in
  let vtr, vtc, vt = opt_mat_alloc prec vt_rows n vt in
  let s, u, vt = I.gesvd ~m ~n ~jobu ~jobvt ?s:(PVec.opt_cnt_vec min_mn s)
                         ~ur ~uc ~u ~vtr ~vtc ~vt
                         ?work:(PVec.opt_work work) ~ar ~ac a in
  ((min_mn, 1, 1, s), (m, u_cols, ur, uc, u), (vt_rows, n, vtr, vtc, vt))

(** {4 gesdd} *)

type ('m, 'n) gesdd_liwork

let gesdd_liwork = I.gesdd_liwork

type ('m, 'n, 'jobz) gesdd_min_lwork

let gesdd_min_lwork ~jobz ~m ~n () =
  I.gesdd_min_lwork ~jobz ~m ~n ()

let gesdd_calc_sizes m n jobz =
  let min_mn = min m n in
  let u_cols, vt_rows = match jobz with
    | `A -> m, n
    | `S -> min_mn, min_mn
    | `O -> m, n
    | `N -> 0, 0 in
  (min_mn, u_cols, vt_rows)

let gesdd_opt_lwork ~jobz ?s ?u ?vt ?iwork (m, n, ar, ac, a) =
  let min_mn, u_cols, vt_rows = gesdd_calc_sizes m n jobz in
  let ur, uc, u = opt_mat m u_cols u in
  let vtr, vtc, vt = opt_mat vt_rows n vt in
  I.gesdd_opt_lwork ~m ~n ~jobz ?s:(PVec.opt_cnt_vec min_mn s)
                    ?ur ?uc ?u ?vtr ?vtc ?vt
                    ?iwork:(PVec.opt_work iwork)
                    ~ar ~ac a
  |> Size.unsafe_of_int

let gesdd ~jobz ?s ?u ?vt ?work ?iwork (m, n, ar, ac, a) =
  let min_mn, u_cols, vt_rows = gesdd_calc_sizes m n jobz in
  let ur, uc, u = opt_mat_alloc prec m u_cols u in
  let vtr, vtc, vt = opt_mat_alloc prec vt_rows n vt in
  let s, u, vt = I.gesdd ~m ~n ~jobz ?s:(PVec.opt_cnt_vec min_mn s)
                         ~ur ~uc ~u ~vtr ~vtc ~vt
                         ?work:(PVec.opt_work work) ?iwork:(PVec.opt_work iwork)
                         ~ar ~ac a in
  ((min_mn, 1, 1, s), (m, u_cols, ur, uc, u), (vt_rows, n, vtr, vtc, vt))

(** {3 General eigenvalue problem (simple drivers)} *)

(** {4 geev} *)

let geev_min_lwork ?vectors n =
  I.geev_min_lwork ?vectors n
  |> Size.unsafe_of_int

let geev_opt_lwork ?vl ?vr ?wr ?wi (n, n', ar, ac, a) =
  assert(n = n');
  let make_arg = function
    | Some (Some (xn, xn', xr, xc, x)) ->
       assert(n = xn && n = xn' && PMat.check_cnt n n xr xc x);
       Some xr, Some xc, Some (Some x)
    | Some None ->
       None, None, Some None
    | None ->
       None, None, None
  in
  let vlr, vlc, vl = make_arg vl in
  let vrr, vrc, vr = make_arg vr in
  I.geev_opt_lwork ~n ?vlr ?vlc ?vl ?vrr ?vrc ?vr
                    ?wr:(PVec.opt_cnt_vec n wr)
                    ?wi:(PVec.opt_cnt_vec n wi)
                    ~ar ~ac a
  |> Size.unsafe_of_int

let geev ?work ?vl ?vr ?wr ?wi (n, n', ar, ac, a) =
  assert(n = n');
  let make_conv_and_arg = function
    | Some (Some (xn, xn', xr, xc, x)) ->
       assert(n = xn && n = xn' && PMat.check_cnt n n xr xc x);
       (fun x -> Some (n, n, xr, xc, x)), Some xr, Some xc, Some (Some x)
    | Some None ->
       (fun x -> Some (n, n, 1, 1, x)), None, None, Some None
    | None ->
       (fun _ -> None), None, None, None
  in
  let conv_vl, vlr, vlc, vl = make_conv_and_arg vl in
  let conv_vr, vrr, vrc, vr = make_conv_and_arg vr in
  let vl, wr, wi, vr = I.geev ~n
                               ?work:(PVec.opt_work work)
                               ?vlr ?vlc ?vl ?vrr ?vrc ?vr
                               ?wr:(PVec.opt_cnt_vec n wr)
                               ?wi:(PVec.opt_cnt_vec n wi)
                               ~ar ~ac a in
  (conv_vl vl), (n, 1, 1, wr), (n, 1, 1, wi), (conv_vr vr)

(** {3 Symmetric-matrix eigenvalue and singular value problems
         (simple drivers)} *)

(** {4 syev} *)

type 'n syev_min_lwork

let syev_min_lwork = I.syev_min_lwork

let syev_opt_lwork ?vectors ?up (n, n', ar, ac, a) =
  assert(n = n');
  I.syev_opt_lwork ~n ?vectors ?up ~ar ~ac a
  |> Size.unsafe_of_int

let syev ?vectors ?up ?w (n, n', ar, ac, a) =
  assert(n = n');
  let w = PVec.opt_cnt_vec_alloc prec n w in
  ignore (I.syev ~n ?vectors ?up ~w ~ar ~ac a);
  (n, 1, 1, w)

(** {4 syevd} *)

let syevd_min_lwork ~vectors n =
  I.syevd_min_lwork ~vectors n
  |> Size.unsafe_of_int

let syevd_min_liwork ~vectors n =
  I.syevd_min_liwork ~vectors n
  |> Size.unsafe_of_int

let syevd_opt_lwork ?vectors ?up (n, n', ar, ac, a) =
  assert(n = n');
  I.syevd_opt_lwork ~n ?vectors ?up ~ar ~ac a
  |> Size.unsafe_of_int

let syevd_opt_liwork ?vectors ?up (n, n', ar, ac, a) =
  assert(n = n');
  I.syevd_opt_liwork ~n ?vectors ?up ~ar ~ac a
  |> Size.unsafe_of_int

let syevd ?vectors ?up ?work ?iwork ?w (n, n', ar, ac, a) =
  assert(n = n');
  let w = I.syevd ~n ?vectors ?up
                   ?work:(PVec.opt_work work)
                   ?iwork:(PVec.opt_work iwork)
                   ?w:(PVec.opt_cnt_vec n w)
                   ~ar ~ac a in
  (n, 1, 1, w)

(** {4 sbev} *)

type 'n sbev_min_lwork

let sbev_min_lwork = I.sbev_min_lwork

let sbev ~kd ?z ?up ?work ?w (sbsize, n, abr, abc, ab) =
  assert(sbsize = Size.syband_dyn n kd);
  let zr, zc, z = PMat.opt_mat_alloc prec n n z in
  let w = I.sbev ~n ~kd ~zr ~zc ~z ?up
                 ?work:(PVec.opt_work work)
                 ?w:(PVec.opt_cnt_vec n w)
                 ~abr ~abc ab in
  (n, 1, 1, w)

(** {3 Symmetric-matrix eigenvalue and singular value problems
       (expert & RRR drivers)} *)

(** {4 syevr} *)


type 'n syevr_min_lwork

let syevr_min_lwork = I.syevr_min_lwork

type 'n syevr_min_liwork

let syevr_min_liwork = I.syevr_min_liwork

let syevr_opt_lwork ?vectors ?range ?up ?abstol (n, n', ar, ac, a) =
  assert(n = n');
  I.syevr_opt_lwork ~n ?vectors ?range ?up ?abstol ~ar ~ac a
  |> Size.unsafe_of_int

let syevr_opt_liwork ?vectors ?range ?up ?abstol (n, n', ar, ac, a) =
  assert(n = n');
  I.syevr_opt_liwork ~n ?vectors ?range ?up ?abstol ~ar ~ac a
  |> Size.unsafe_of_int

module type SYEVR_RESULT =
  sig
    type m
    type n
    val value : m Size.t *
                  (n, 'cnt) vec *
                    (n, m, 'cnt) mat *
                      ((m, m) Size.add, 'cnt) Common.int32_vec
  end

type 'n syevr_result = (module SYEVR_RESULT with type n = 'n)

let syevr_dyn (type nn) ?vectors ?range ?up ?abstol ?work ?iwork ?w ?z ?isuppz
              ((n, n', ar, ac, a) : (nn, nn, 'a_cd) mat) =
  assert(n = n');
  let syevr k ?zr ?zc ?z ar ac a =
    I.syevr ~n ?vectors ?range ?up ?abstol
            ?work:(PVec.opt_work work)
            ?iwork:(PVec.opt_work iwork)
            ?w:(PVec.opt_cnt_vec n w)
            ?isuppz:(PVec.opt_cnt_vec (2 * k) isuppz)
            ?zr ?zc ?z ~ar ~ac a in
  let res = match z with
    | Some (n'', k, zr, zc, z) ->
       assert(n = n'');
       let (m, w, z, isuppz) = syevr k ~zr ~zc ~z ar ac a in
       if m > k
       then invalid_argf "syevr_dyn: m (= %d) should be smaller than k (= %d)"
                         m k ();
       (m, (n, 1, 1, w), (n, m, zr, zc, z), (2 * m, 1, 1, isuppz))
    | None ->
       let (m, w, z, isuppz) = syevr n ar ac a in
       (m, (n, 1, 1, w), (n, m, 1, 1, z), (2 * m, 1, 1, isuppz)) in
  let module R =
    struct
      type n = nn
      type m
      let value = res
    end in
  (module R : SYEVR_RESULT with type n = nn)

(** {4 sygv} *)

let sygv_opt_lwork ?vectors ?up ?itype (n, n', ar, ac, a)
                   (n'', n''', br, bc, b) =
  assert(n = n' && n = n'' && n = n''');
  I.sygv_opt_lwork ~n ?vectors ?up ?itype ~ar ~ac a ~br ~bc b
  |> Size.unsafe_of_int

let sygv ?vectors ?up ?work ?w ?itype (n, n', ar, ac, a)
         (n'', n''', br, bc, b) =
  assert(n = n' && n = n'' && n = n''');
  let w = I.sygv ~n ?vectors ?up ?work:(PVec.opt_work work)
                 ?w:(PVec.opt_cnt_vec n w) ?itype ~ar ~ac a ~br ~bc b in
  (n, 1, 1, w)

(** {4 sbgv} *)

let sbgv ~ka ~kb ?z ?up ?work ?w
         (sbsize_a, n, abr, abc, ab) (sbsize_b, n', bbr, bbc, bb) =
  assert(n = n');
  assert(sbsize_a = Size.syband_dyn n ka);
  assert(sbsize_b = Size.syband_dyn n kb);
  let zr, zc, z = PMat.opt_mat_alloc prec n n z in
  let w = I.sbgv ~n ~ka ~kb ~zr ~zc ~z ?up
                 ?work:(PVec.opt_work work)
                 ?w:(PVec.opt_cnt_vec n w)
                 ~ar:abr ~ac:abc ab ~br:bbr ~bc:bbc bb in
  (n, 1, 1, w)
