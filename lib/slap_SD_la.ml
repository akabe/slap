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

open Bigarray

(** {2 BLAS interface} *)

(** {3 Level 1} *)

(* DOT *)

external direct_dot :
  n : _ Slap_size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  float = "lacaml_XSDCZdot_stub_bc" "lacaml_XSDCZdot_stub"

let dot x y =
  let n, incx, x = Slap_vec.__expose x in
  let n', incy, y = Slap_vec.__expose y in
  assert(n = n');
  direct_dot ~n ~ofsx:1 ~incx ~x ~ofsy:1 ~incy ~y


(* ASUM *)

external direct_asum :
  n : _ Slap_size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  float = "lacaml_XSDCZasum_stub"

let asum x =
  let n, incx, x = Slap_vec.__expose x in
  direct_asum ~n ~ofsx:1 ~incx ~x

(** {3 Level 2} *)

(* SBMV *)

external direct_sbmv :
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Slap_size.t ->
  k : _ Slap_size.t ->
  up : [< `U | `L ] Slap_common.uplo ->
  alpha : float ->
  beta : float ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZsbmv_stub_bc" "lacaml_XSDCZsbmv_stub"

let sbmv ~k ?y a ?(up = Slap_common.__unexpose_uplo 'U')
    ?(alpha = 1.0) ?(beta = 0.0) x =
  let sbs, n, ar, ac, a = Slap_mat.__expose a in
  let n', incx, x = Slap_vec.__expose x in
  assert(n = n' && sbs = Slap_size.syband_dyn n k);
  let incy, y = Slap_vec.opt_vec_alloc prec n y in
  if Slap_size.nonzero n && Slap_size.nonzero k
  then direct_sbmv ~ofsy:1 ~incy ~y ~ar ~ac ~a ~n ~k ~up ~alpha ~beta
      ~ofsx:1 ~incx ~x;
  Slap_vec.__unexpose n incy y


(* GER *)

external direct_ger :
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  alpha : float ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  unit = "lacaml_XSDCZger_stub_bc" "lacaml_XSDCZger_stub"

let ger ?(alpha = 1.0) x y a =
  let m, incx, x = Slap_vec.__expose x in
  let n, incy, y = Slap_vec.__expose y in
  let m', n', ar, ac, a = Slap_mat.__expose a in
  assert(m = m' && n = n');
  if Slap_size.nonzero m && Slap_size.nonzero n
  then direct_ger ~m ~n ~alpha ~ofsx:1 ~incx ~x ~ofsy:1 ~incy ~y ~ar ~ac ~a;
  Slap_mat.__unexpose m n ar ac a


(* SYR *)

external direct_syr :
  up : [< `U | `L ] Slap_common.uplo ->
  n : _ Slap_size.t ->
  alpha : float ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  unit = "lacaml_XSDCZsyr_stub_bc" "lacaml_XSDCZsyr_stub"

let syr ?(alpha = 1.0) ?(up = Slap_common.__unexpose_uplo 'U') x a =
  let n, incx, x = Slap_vec.__expose x in
  let n', n'', ar, ac, a = Slap_mat.__expose a in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n
  then direct_syr ~up ~n ~alpha ~ofsx:1 ~incx ~x ~ar ~ac ~a;
  Slap_mat.__unexpose n n ar ac a

(** {2 LAPACK interface} *)

(** {3 Auxiliary routines} *)

(* LANSY *)

external direct_lansy :
  norm : (_, _) Slap_common.norm ->
  up : [< `U | `L ] Slap_common.uplo ->
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  float = "lacaml_XSDCZlansy_stub_bc" "lacaml_XSDCZlansy_stub"

type ('m, 'a) lansy_min_lwork

let lansy_min_lwork n norm =
  let lwork = match Slap_common.__expose_norm norm with
    | 'I' | 'O' -> Slap_size.__expose n
    | _ -> 0 in
  Slap_size.__unexpose lwork

let lansy
    ?(up = Slap_common.__unexpose_uplo 'U')
    ?(norm = Slap_common.__unexpose_norm 'O')
    ?work a =
  let n, n', ar, ac, a = Slap_mat.__expose a in
  assert(n = n');
  if Slap_size.nonzero n then begin
    let min_lwork = lansy_min_lwork n norm in
    let _, work = Slap_vec.__alloc_work prec work ~loc:"Slap.XSDCZ.lansy"
        ~min_lwork  ~opt_lwork:min_lwork in
    direct_lansy ~norm ~up ~n ~ar ~ac ~a ~work
  end
  else 0.0

(* LAMCH *)

external direct_lamch : char -> float = "lacaml_XSDCZlamch_stub"

let lamch cmach =
  direct_lamch
    (match cmach with
     | `N -> 'N'
     | `O -> 'O'
     | `P -> 'P'
     | `R -> 'R'
     | `S -> 'S'
     | `U -> 'U'
     | `L -> 'L'
     | `M -> 'M'
     | `B -> 'B'
     | `E -> 'E')

(** {3 Linear equations (computational routines)} *)

(** {4 orgqr} *)

external direct_orgqr :
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  k : _ Slap_size.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  tau : ('a, 'b, fortran_layout) Array1.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZorgqr_stub_bc" "lacaml_XSDCZorgqr_stub"

type 'n orgqr_min_lwork = (Slap_size.one, 'n) Slap_size.max

let orgqr_min_lwork ~n = Slap_size.max Slap_size.one n

let orgqr_check_size loc k m n =
  let k = Slap_size.__expose k in
  let m = Slap_size.__expose m in
  let n = Slap_size.__expose n in
  if m < n
  then Slap_misc.invalid_argf "%s: dim1(a)=%d, dim2(a)=%d \
                               (unsatisfied: dim1(a) >= dim2(a))" loc m n ();
  if n < k
  then Slap_misc.invalid_argf "%s: dim2(a)=%d, dim(tau)=%d \
                               (unsatisfied: dim2(a) >= dim(tau))" loc n k ()

let orgqr_opt_lwork_aux ~tau a =
  assert(Slap_vec.check_cnt tau);
  let k, _, tau = Slap_vec.__expose tau in
  let m, n, ar, ac, a = Slap_mat.__expose a in
  orgqr_check_size "orgqr_opt_lwork" k m n;
  let work = Array1.create prec fortran_layout 1 in
  let i = direct_orgqr ~m ~n ~k ~work ~lwork:(-1) ~tau ~ar ~ac ~a in
  if i = 0 then Slap_size.__unexpose (int_of_float work.{1})
  else internal_error "Slap.XSDCZ.orgqr_opt_lwork" i

let orgqr_opt_lwork ~tau a =
  orgqr_opt_lwork_aux ~tau a
  |> Slap_size.__expose
  |> Slap_size.unsafe_of_int

let orgqr_dyn ?work ~tau a =
  assert(Slap_vec.check_cnt tau);
  let k, _, ba_tau = Slap_vec.__expose tau in
  let m, n, ar, ac, ba_a = Slap_mat.__expose a in
  orgqr_check_size "orgqr_dyn" k m n;
  if Slap_size.nonzero k && Slap_size.nonzero m && Slap_size.nonzero n
  then begin
    let loc = "Slap.XSDCZ.orgqr_dyn" in
    let lwork, work = Slap_vec.__alloc_work prec work ~loc
        ~min_lwork:(orgqr_min_lwork ~n)
        ~opt_lwork:(orgqr_opt_lwork_aux ~tau a) in
    let i = direct_orgqr ~m ~n ~k ~work ~lwork ~tau:ba_tau ~ar ~ac ~a:ba_a in
    if i <> 0 then internal_error loc i
  end

(** {4 ormqr} *)

external direct_ormqr :
  side : (_, _, _) Slap_common.side ->
  trans : (_, _) Slap_common.trans ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  k : _ Slap_size.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  tau : ('a, 'b, fortran_layout) Array1.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  cr : int ->
  cc : int ->
  c : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZormqr_stub_bc" "lacaml_XSDCZormqr_stub"

let check_ormqr ~loc ~side ~r ~m ~n ~k ~k' =
  assert(k = k');
  let r = Slap_size.__expose r in
  let m = Slap_size.__expose m in
  let n = Slap_size.__expose n in
  let k = Slap_size.__expose k in
  assert(match Slap_common.__expose_side side with
      | 'L' -> r = m
      | _ -> r = n);
  if k > m
  then Slap_misc.invalid_argf "%s: dim(tau)=%d, dim1(c)=%d \
                               (unsatisfied: dim(tau) <= dim1(c))" loc k m ()

type ('r, 'm, 'n) ormqr_min_lwork

let ormqr_min_lwork ~side ~m ~n =
  begin
    match Slap_common.__expose_side side with
    | 'L' -> max 1 (Slap_size.__expose n)
    | _ -> max 1 (Slap_size.__expose m)
  end
  |> Slap_size.__unexpose

let ormqr_opt_lwork_aux ~side ~trans ~tau a c =
  assert(Slap_vec.check_cnt tau);
  let loc = "Slap.XSDCZ.ormqr_opt_lwork" in
  let k, _, tau = Slap_vec.__expose tau in
  let r, k', ar, ac, a = Slap_mat.__expose a in
  let m, n, cr, cc, c = Slap_mat.__expose c in
  check_ormqr ~loc ~side ~r ~m ~n ~k ~k';
  let work = Array1.create prec fortran_layout 1 in
  let i = direct_ormqr ~side ~trans ~m ~n ~k
      ~work ~lwork:(-1) ~tau ~ar ~ac ~a ~cr ~cc ~c in
  if i = 0 then Slap_size.__unexpose (int_of_float work.{1})
  else internal_error loc i

let ormqr_opt_lwork ~side ~trans ~tau a c =
  ormqr_opt_lwork_aux ~side ~trans ~tau a c
  |> Slap_size.__expose
  |> Slap_size.unsafe_of_int

let ormqr_dyn ~side ~trans ?work ~tau a c =
  assert(Slap_vec.check_cnt tau);
  let loc = "Slap.XSDCZ.ormqr" in
  let k, _, tau' = Slap_vec.__expose tau in
  let r, k', ar, ac, a' = Slap_mat.__expose a in
  let m, n, cr, cc, c' = Slap_mat.__expose c in
  check_ormqr ~loc:"ormqr" ~side ~r ~m ~n ~k ~k';
  if Slap_size.nonzero r && Slap_size.nonzero m
     && Slap_size.nonzero n && Slap_size.nonzero k
  then begin
    let lwork, work =
      Slap_vec.__alloc_work prec work ~loc
        ~min_lwork:(ormqr_min_lwork ~side ~m ~n)
        ~opt_lwork:(ormqr_opt_lwork_aux ~side ~trans ~tau a c) in
    let i = direct_ormqr ~side ~trans ~m ~n ~k
        ~work ~lwork ~tau:tau' ~ar ~ac ~a:a' ~cr ~cc ~c:c' in
    if i <> 0 then internal_error loc i
  end

(** {4 gecon} *)

external direct_gecon :
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  iwork : (int32, int32_elt, fortran_layout) Array1.t ->
  norm : (_, _) Slap_common.norm ->
  anorm : float ->
  int * float = "lacaml_XSDCZgecon_stub_bc" "lacaml_XSDCZgecon_stub"

type 'n gecon_min_lwork = (Slap_size.four, 'n) Slap_size.mul

let gecon_min_lwork n = Slap_size.mul Slap_size.four n

type 'n gecon_min_liwork = 'n

let gecon_min_liwork n = n

let gecon ?(norm = Slap_common.__unexpose_norm 'O') ?anorm ?work ?iwork aa =
  let n, n', ar, ac, a = Slap_mat.__expose aa in
  assert(n = n');
  if Slap_size.nonzero n then begin
    let loc = "Slap.XSDCZ.gecon" in
    let min_lwork = gecon_min_lwork n in
    let _, work = Slap_vec.__alloc_work prec work ~loc
        ~min_lwork ~opt_lwork:min_lwork in
    let min_liwork = gecon_min_liwork n in
    let _, iwork = Slap_vec.__alloc_work int32 iwork ~loc
        ~min_lwork:min_liwork ~opt_lwork:min_liwork in
    let anorm = match anorm with
      | None -> lange ~norm aa
      | Some anorm -> anorm in
    let i, res = direct_gecon ~n ~ar ~ac ~a ~work ~iwork ~norm ~anorm in
    if i = 0 then res
    else internal_error loc i
  end else 0.0


(** {4 sycon} *)

external direct_sycon :
  up : [< `U | `L ] Slap_common.uplo ->
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  iwork : (int32, int32_elt, fortran_layout) Array1.t ->
  anorm : float ->
  int * float = "lacaml_XSDCZsycon_stub_bc" "lacaml_XSDCZsycon_stub"

type 'n sycon_min_lwork = (Slap_size.two, 'n) Slap_size.mul

let sycon_min_lwork n = Slap_size.mul Slap_size.two n

type 'n sycon_min_liwork = 'n

let sycon_min_liwork n = n

let sycon ?(up = Slap_common.__unexpose_uplo 'U') ?ipiv ?anorm ?work ?iwork aa =
  let n, n', ar, ac, a = Slap_mat.__expose aa in
  assert(n = n');
  if Slap_size.nonzero n then begin
    let loc = "Slap.XSDCZ.sycon" in
    let min_lwork = sycon_min_lwork n in
    let _, work = Slap_vec.__alloc_work prec work ~loc
        ~min_lwork ~opt_lwork:min_lwork in
    let min_liwork = sycon_min_liwork n in
    let _, iwork = Slap_vec.__alloc_work int32 iwork ~loc
        ~min_lwork:min_liwork ~opt_lwork:min_liwork in
    let ipiv =
      if ipiv = None
      then sytrf ~up ~work:(Slap_vec.__unexpose min_lwork 1 work) aa
           |> Slap_vec.__expose
           |> (fun (_, _, ipiv) -> ipiv)
      else Slap_vec.opt_cnt_vec_alloc int32 n ipiv in
    let anorm = match anorm with
      | None -> lange aa
      | Some anorm -> anorm in
    let i, res = direct_sycon ~up ~n ~ar ~ac ~a ~ipiv ~work ~iwork ~anorm in
    if i = 0 then res else internal_error loc i
  end else 0.0

(** {4 pocon} *)

external direct_pocon :
  up : [< `U | `L ] Slap_common.uplo ->
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  iwork : (int32, int32_elt, fortran_layout) Array1.t ->
  anorm : float ->
  int * float = "lacaml_XSDCZpocon_stub_bc" "lacaml_XSDCZpocon_stub"

type 'n pocon_min_lwork = (Slap_size.three, 'n) Slap_size.mul

let pocon_min_lwork n = Slap_size.mul Slap_size.three n

type 'n pocon_min_liwork = 'n

let pocon_min_liwork n = n

let pocon ?(up = Slap_common.__unexpose_uplo 'U') ?anorm ?work ?iwork aa =
  let n, n', ar, ac, a = Slap_mat.__expose aa in
  assert(n = n');
  if Slap_size.nonzero n then begin
    let loc = "Slap.XSDCZ.pocon" in
    let min_lwork = pocon_min_lwork n in
    let _, work = Slap_vec.__alloc_work prec work ~loc
        ~min_lwork ~opt_lwork:min_lwork in
    let min_liwork = pocon_min_liwork n in
    let _, iwork = Slap_vec.__alloc_work int32 iwork ~loc
        ~min_lwork:min_liwork ~opt_lwork:min_liwork in
    let anorm = match anorm with
      | None -> lange aa
      | Some anorm -> anorm in
    let i, res = direct_pocon ~up ~n ~ar ~ac ~a ~work ~iwork ~anorm in
    if i = 0 then res else internal_error loc i
  end else 0.0

(** {3 Least squares (expert drivers)} *)

let gelsx_err loc i =
  assert(i <> 0);
  if i > 0
  then Slap_misc.failwithf "%s: failed to converge on off-diagonal \
                            element %d" loc i ()
  else internal_error loc i

(** {4 gelsy} *)

external direct_gelsy :
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  jpvt : (int32, int32_elt, fortran_layout) Array1.t ->
  rcond : float ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  nrhs : _ Slap_size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int * int = "lacaml_XSDCZgelsy_stub_bc" "lacaml_XSDCZgelsy_stub"

type ('m, 'n, 'nrhs) gelsy_min_lwork =
  (((('m, 'n) Slap_size.min,
     (Slap_size.three, 'n) Slap_size.mul) Slap_size.add,
    Slap_size.one)
     Slap_size.add,
   ((Slap_size.two, ('m, 'n) Slap_size.min) Slap_size.mul,
    'nrhs) Slap_size.add)
    Slap_size.max

let gelsy_min_lwork ~m ~n ~nrhs =
  let open Slap_size in
  let ( + ) = add in
  let ( * ) = mul in
  let min_mn = min m n in
  max (min_mn + three * n + one) (two * min_mn + nrhs)

let gelsy_opt_lwork_aux a b =
  let m, n, ar, ac, a = Slap_mat.__expose a in
  let n', nrhs, br, bc, b = Slap_mat.__expose b in
  assert(n = n');
  let work = Array1.create prec fortran_layout 1 in
  let jpvt = Array1.create int32 fortran_layout 0 in
  let i, _ =
    direct_gelsy ~ar ~ac ~a ~m ~n ~jpvt
      ~rcond:(-1.0) ~work ~lwork:(-1) ~nrhs ~br ~bc ~b in
  if i = 0 then int_of_float work.{1} |> Slap_size.__unexpose
  else gelsx_err "Slap.XSDCZ.gelsy_opt_lwork" i

let gelsy_opt_lwork a b =
  gelsy_opt_lwork_aux a b
  |> Slap_size.__expose
  |> Slap_size.unsafe_of_int

let gelsy aa ?(rcond = -1.0) ?jpvt ?work bb =
  let m, n, ar, ac, a = Slap_mat.__expose aa in
  let n', nrhs, br, bc, b = Slap_mat.__expose bb in
  assert(n = n');
  if Slap_size.nonzero m && Slap_size.nonzero n && Slap_size.nonzero nrhs
  then begin
    let loc = "Slap.XSDCZ.gelsy" in
    let jpvt = match jpvt with
      | Some jpvt ->
        assert(Slap_vec.check_cnt jpvt);
        let n'', _, jpvt = Slap_vec.__expose jpvt in
        assert(n = n'');
        jpvt
      | None ->
        let jpvt = Array1.create int32 fortran_layout (Slap_size.to_int n) in
        Array1.fill jpvt Int32.zero;
        jpvt in
    let lwork, work = Slap_vec.__alloc_work prec work ~loc
        ~min_lwork:(gelsy_min_lwork ~m ~n ~nrhs)
        ~opt_lwork:(gelsy_opt_lwork_aux aa bb) in
    let i, rank =
      direct_gelsy ~ar ~ac ~a ~m ~n
        ~jpvt ~rcond ~work ~lwork ~nrhs ~br ~bc ~b in
    if i = 0 then rank
    else gelsx_err loc i
  end else 0

(** {4 gelsd} *)

external direct_gelsd :
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  ofss : int ->
  s : ('a, 'b, fortran_layout) Array1.t ->
  rcond : float ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  iwork : ('a, 'b, fortran_layout) Array1.t ->
  nrhs : _ Slap_size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int * int = "lacaml_XSDCZgelsd_stub_bc" "lacaml_XSDCZgelsd_stub"

type ('m, 'n, 'nrhs) gelsd_min_lwork

let gelsd_min_lwork ~m ~n ~nrhs =
  Lacaml.XSDCZ.gelsd_min_lwork
    ~m:(Slap_size.__expose m) ~n:(Slap_size.__expose n)
    ~nrhs:(Slap_size.__expose nrhs)
  |> Slap_size.__unexpose

let gelsd_opt_lwork_aux a b =
  let m, n, ar, ac, a = Slap_mat.__expose a in
  let n', nrhs, br, bc, b = Slap_mat.__expose b in
  assert(n = n');
  let work = Array1.create prec fortran_layout 1 in
  let empty = Array1.create prec fortran_layout 0 in
  let i, _ =
    direct_gelsd ~ar ~ac ~a ~m ~n ~ofss:1 ~s:empty ~rcond:(-1.0)
      ~work ~lwork:(-1) ~iwork:empty ~nrhs ~br ~bc ~b in
  if i = 0 then
    (* FIXME: Lacaml says this code has a bug of LAPACK maybe. *)
    Slap_size.max
      (Slap_size.__unexpose (int_of_float work.{1}))
      (gelsd_min_lwork ~m ~n ~nrhs)
  else gelsx_err "Slap.XSDCZ.gelsd_opt_lwork" i

let gelsd_opt_lwork a b =
  gelsd_opt_lwork_aux a b
  |> Slap_size.__expose
  |> Slap_size.unsafe_of_int

type ('m, 'n, 'nrhs) gelsd_min_iwork

let gelsd_min_iwork m n =
  Lacaml.XSDCZ.gelsd_min_iwork (Slap_size.__expose m) (Slap_size.__expose n)
  |> Slap_size.__unexpose

let gelsd aa ?(rcond = -1.0) ?s ?work ?iwork bb =
  let m, n, ar, ac, a = Slap_mat.__expose aa in
  let n', nrhs, br, bc, b = Slap_mat.__expose bb in
  assert(n = n');
  let mn = Slap_size.min m n in
  if Slap_size.nonzero mn && Slap_size.nonzero nrhs then begin
    let loc = "Slap.XSDCZ.gelsd" in
    let s = Slap_vec.opt_cnt_vec_alloc prec mn s in
    let min_liwork = gelsd_min_iwork m n in
    let _, iwork = Slap_vec.__alloc_work prec work ~loc
        ~min_lwork:min_liwork ~opt_lwork:min_liwork in
    let lwork, work = Slap_vec.__alloc_work prec work ~loc
        ~min_lwork:(gelsd_min_lwork ~m ~n ~nrhs)
        ~opt_lwork:(gelsd_opt_lwork_aux aa bb) in
    let i, rank =
      direct_gelsd ~ar ~ac ~a ~m ~n ~ofss:1 ~s ~rcond
        ~work ~lwork ~iwork ~nrhs ~br ~bc ~b in
    if i = 0 then rank else gelsx_err loc i
  end else 0

(** {4 gelss} *)

external direct_gelss :
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  ofss : int ->
  s : ('a, 'b, fortran_layout) Array1.t ->
  rcond : float ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  nrhs : _ Slap_size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int * int = "lacaml_XSDCZgelss_stub_bc" "lacaml_XSDCZgelss_stub"

type ('m, 'n, 'nrhs) gelss_min_lwork

let gelss_min_lwork ~m ~n ~nrhs =
  Lacaml.XSDCZ.gelss_min_lwork
    ~m:(Slap_size.__expose m) ~n:(Slap_size.__expose n)
    ~nrhs:(Slap_size.__expose nrhs)
  |> Slap_size.__unexpose

let gelss_opt_lwork_aux a b =
  let m, n, ar, ac, a = Slap_mat.__expose a in
  let n', nrhs, br, bc, b = Slap_mat.__expose b in
  assert(n = n');
  let work = Array1.create prec fortran_layout 1 in
  let i, _ =
    direct_gelss ~ar ~ac ~a ~m ~n ~ofss:1 ~s:Lacaml.XSDCZ.Vec.empty
      ~rcond:(-1.0) ~work ~lwork:(-1) ~nrhs ~br ~bc ~b in
  if i = 0 then int_of_float work.{1} |> Slap_size.__unexpose
  else gelsx_err "Slap.XSDCZ.gelss_opt_lwork" i

let gelss_opt_lwork a b =
  gelss_opt_lwork_aux a b
  |> Slap_size.__expose
  |> Slap_size.unsafe_of_int

let gelss aa ?(rcond = -1.0) ?s ?work bb =
  let m, n, ar, ac, a = Slap_mat.__expose aa in
  let n', nrhs, br, bc, b = Slap_mat.__expose bb in
  assert(n = n');
  let mn = Slap_size.min m n in
  if Slap_size.nonzero mn && Slap_size.nonzero nrhs
  then begin
    let loc = "Slap.XSDCZ.gelss" in
    let s = Slap_vec.opt_cnt_vec_alloc prec mn s in
    let lwork, work = Slap_vec.__alloc_work prec work ~loc
        ~min_lwork:(gelss_min_lwork ~m ~n ~nrhs)
        ~opt_lwork:(gelss_opt_lwork_aux aa bb) in
    let i, rank =
      direct_gelss ~ar ~ac ~a ~m ~n ~ofss:1 ~s ~rcond
        ~work ~lwork ~nrhs ~br ~bc ~b in
    if i = 0 then rank else gelsx_err loc i
  end else 0

(** {3 General SVD routines} *)

(** {4 gesvd} *)

type ('m, 'n) gesvd_min_lwork

let gesvd_min_lwork ~m ~n =
  S.__unexpose (I.gesvd_min_lwork ~m:(S.__expose m) ~n:(S.__expose n))

let gesvd_calc_sizes m n jobu jobvt =
  let min_mn = Slap_size.min m n in
  let job_size c = function
    | `A -> S.__expose c
    | `S -> S.__expose min_mn
    | `O | `N -> 0 in
  let u_cols = S.__unexpose (job_size m (lacaml_svd_job jobu)) in
  let vt_rows = S.__unexpose (job_size n (lacaml_svd_job jobvt)) in
  (min_mn, u_cols, vt_rows)

let gesvd_opt_lwork ~jobu ~jobvt ?s ?u ?vt a =
  let m, n, ar, ac, a = M.__expose a in
  let min_mn, u_cols, vt_rows = gesvd_calc_sizes m n jobu jobvt in
  let ur, uc, u = Slap_mat.opt_mat m u_cols u in
  let vtr, vtc, vt = Slap_mat.opt_mat vt_rows n vt in
  I.gesvd_opt_lwork ~m:(S.__expose m) ~n:(S.__expose n)
    ~jobu:(lacaml_svd_job jobu) ~jobvt:(lacaml_svd_job jobvt)
    ?s:(Slap_vec.opt_cnt_vec min_mn s) ?ur ?uc ?u ?vtr ?vtc ?vt ~ar ~ac a
  |> Slap_size.unsafe_of_int

let gesvd ~jobu ~jobvt ?s ?u ?vt ?work a =
  let m, n, ar, ac, a = M.__expose a in
  let min_mn, u_cols, vt_rows = gesvd_calc_sizes m n jobu jobvt in
  let ur, uc, u = Slap_mat.opt_mat_alloc prec m u_cols u in
  let vtr, vtc, vt = Slap_mat.opt_mat_alloc prec vt_rows n vt in
  let s, u, vt = I.gesvd ~m:(S.__expose m) ~n:(S.__expose n)
      ~jobu:(lacaml_svd_job jobu) ~jobvt:(lacaml_svd_job jobvt)
      ?s:(Slap_vec.opt_cnt_vec min_mn s) ~ur ~uc ~u ~vtr ~vtc ~vt
      ?work:(Slap_vec.opt_work work) ~ar ~ac a in
  (V.__unexpose min_mn 1 s,
   M.__unexpose m u_cols ur uc u,
   M.__unexpose vt_rows n vtr vtc vt)

(** {4 gesdd} *)

type ('m, 'n) gesdd_liwork

let gesdd_liwork ~m ~n =
  S.__unexpose (I.gesdd_liwork ~m:(S.__expose m) ~n:(S.__expose n))

type ('m, 'n, 'jobz) gesdd_min_lwork

let gesdd_min_lwork ~jobz ~m ~n () =
  S.__unexpose (I.gesdd_min_lwork ~jobz:(lacaml_svd_job jobz)
                    ~m:(S.__expose m) ~n:(S.__expose n) ())

let gesdd_calc_sizes m n jobz =
  let m = S.__expose m in
  let n = S.__expose n in
  match lacaml_svd_job jobz with
  | `A -> (m, n)
  | `S -> let k = min m n in (k, k)
  | `O -> if m >= n then (0, n) else (m, 0)
  | `N -> (0, 0)

let gesdd_opt_lwork ~jobz ?s ?u ?vt ?iwork a =
  let m, n, ar, ac, a = M.__expose a in
  let min_mn = Slap_size.min m n in
  let u_cols, vt_rows = gesdd_calc_sizes m n jobz in
  let ur, uc, u = Slap_mat.opt_mat m (S.__unexpose u_cols) u in
  let vtr, vtc, vt = Slap_mat.opt_mat (S.__unexpose vt_rows) n vt in
  I.gesdd_opt_lwork ~m:(S.__expose m) ~n:(S.__expose m)
    ~jobz:(lacaml_svd_job jobz) ?s:(Slap_vec.opt_cnt_vec min_mn s)
    ?iwork:(Slap_vec.opt_work iwork)
    ?ur ?uc ?u ?vtr ?vtc ?vt ~ar ~ac a
  |> Slap_size.unsafe_of_int

let gesdd ~jobz ?s ?u ?vt ?work ?iwork a =
  let m, n, ar, ac, a = M.__expose a in
  let min_mn = Slap_size.min m n in
  let _gesdd ?ur ?uc ?u ?vtr ?vtc ?vt () =
    I.gesdd ~m:(S.__expose m) ~n:(S.__expose n) ~jobz:(lacaml_svd_job jobz)
      ?s:(Slap_vec.opt_cnt_vec min_mn s) ?work:(Slap_vec.opt_work work)
      ?iwork:(Slap_vec.opt_work iwork) ?ur ?uc ?u ?vtr ?vtc ?vt ~ar ~ac a
  in
  let opt_mat_alloc m n a = Slap_mat.opt_mat_alloc prec m n a in
  let mks s = V.__unexpose min_mn 1 s in
  let mku u_cols ur uc u =
    Some (M.__unexpose m (S.__unexpose u_cols) ur uc u) in
  let mkvt vt_rows vtr vtc vt =
    Some (M.__unexpose (S.__unexpose vt_rows) n vtr vtc vt) in
  match gesdd_calc_sizes m n jobz with
  | 0, 0 ->
    let s, _, _ = _gesdd () in
    (mks s, None, None)
  | 0, vt_rows ->
    let vtr, vtc, vt = opt_mat_alloc (S.__unexpose vt_rows) n vt in
    let s, _, vt = _gesdd ~vtr ~vtc ~vt () in
    (mks s, None, mkvt vt_rows vtr vtc vt)
  | u_cols, 0 ->
    let ur, uc, u = opt_mat_alloc m (S.__unexpose u_cols) u in
    let s, u, _ = _gesdd ~ur ~uc ~u () in
    (mks s, mku u_cols ur uc u, None)
  | u_cols, vt_rows ->
    let ur, uc, u = opt_mat_alloc m (S.__unexpose u_cols) u in
    let vtr, vtc, vt = opt_mat_alloc (S.__unexpose vt_rows) n vt in
    let s, u, vt = _gesdd ~ur ~uc ~u ~vtr ~vtc ~vt () in
    (mks s, mku u_cols ur uc u, mkvt vt_rows vtr vtc vt)

(** {3 General eigenvalue problem (simple drivers)} *)

(** {4 geev} *)

let geev_min_lwork ?vectors n =
  I.geev_min_lwork ?vectors (S.__expose n)
  |> Slap_size.unsafe_of_int

let geev_opt_lwork ?vl ?vr ?wr ?wi a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  let make_arg = function
    | Some (Some x) ->
      let xn, xn', xr, xc, x = M.__expose x in
      assert(n = xn && n = xn');
      Some xr, Some xc, Some (Some x)
    | Some None -> None, None, Some None
    | None -> None, None, None
  in
  let vlr, vlc, vl = make_arg vl in
  let vrr, vrc, vr = make_arg vr in
  I.geev_opt_lwork ~n:(S.__expose n) ?vlr ?vlc ?vl ?vrr ?vrc ?vr
    ?wr:(Slap_vec.opt_cnt_vec n wr) ?wi:(Slap_vec.opt_cnt_vec n wi) ~ar ~ac a
  |> Slap_size.unsafe_of_int

let geev ?work ?vl ?vr ?wr ?wi a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  let make_conv_and_arg = function
    | Some (Some x) ->
      let xn, xn', xr, xc, x = M.__expose x in
      assert(n = xn && n = xn');
      let conv x = Some (M.__unexpose n n xr xc x) in
      (conv, Some xr, Some xc, Some (Some x))
    | Some None ->
      let conv x = Some (M.__unexpose n n 1 1 x) in
      (conv, None, None, Some None)
    | None -> ((fun _ -> None), None, None, None)
  in
  let conv_vl, vlr, vlc, vl = make_conv_and_arg vl in
  let conv_vr, vrr, vrc, vr = make_conv_and_arg vr in
  let vl, wr, wi, vr = I.geev ~n:(S.__expose n)
      ?work:(Slap_vec.opt_work work) ?vlr ?vlc ?vl ?vrr ?vrc ?vr
      ?wr:(Slap_vec.opt_cnt_vec n wr) ?wi:(Slap_vec.opt_cnt_vec n wi)
      ~ar ~ac a in
  let vl = conv_vl vl in
  let vr = conv_vr vr in
  let wr = V.__unexpose n 1 wr in
  let wi = V.__unexpose n 1 wi in
  (vl, wr, wi, vr)

(** {3 Symmetric-matrix eigenvalue and singular value problems
         (simple drivers)} *)

(** {4 syev} *)

type 'n syev_min_lwork

let syev_min_lwork n = S.__unexpose (I.syev_min_lwork (S.__expose n))

let syev_opt_lwork ?vectors ?up a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  I.syev_opt_lwork ~n:(S.__expose n) ?vectors ?up ~ar ~ac a
  |> Slap_size.unsafe_of_int

let syev ?vectors ?up ?work ?w a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  let w = Slap_vec.opt_cnt_vec_alloc prec n w in
  ignore (I.syev ~n:(S.__expose n) ?vectors ?up
            ?work:(Slap_vec.opt_work work) ~w ~ar ~ac a);
  V.__unexpose n 1 w

(** {4 syevd} *)

let syevd_min_lwork ~vectors n =
  I.syevd_min_lwork ~vectors (S.__expose n)
  |> Slap_size.unsafe_of_int

let syevd_min_liwork ~vectors n =
  I.syevd_min_liwork ~vectors (S.__expose n)
  |> Slap_size.unsafe_of_int

let syevd_opt_lwork ?vectors ?up a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  I.syevd_opt_lwork ~n:(S.__expose n) ?vectors ?up ~ar ~ac a
  |> Slap_size.unsafe_of_int

let syevd_opt_liwork ?vectors ?up a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  I.syevd_opt_liwork ~n:(S.__expose n) ?vectors ?up ~ar ~ac a
  |> Slap_size.unsafe_of_int

let syevd ?vectors ?up ?work ?iwork ?w a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  let w = I.syevd ~n:(S.__expose n) ?vectors ?up
      ?work:(Slap_vec.opt_work work) ?iwork:(Slap_vec.opt_work iwork)
      ?w:(Slap_vec.opt_cnt_vec n w) ~ar ~ac a in
  V.__unexpose n 1 w

(** {4 sbev} *)

type 'n sbev_min_lwork

let sbev_min_lwork n =
  S.__unexpose (I.sbev_min_lwork (S.__expose n))

let sbev ~kd ?z ?up ?work ?w ab =
  let sbsize, n, abr, abc, ab = M.__expose ab in
  assert(sbsize = Slap_size.syband_dyn n kd);
  let zr, zc, z = Slap_mat.opt_mat_alloc prec n n z in
  let w = I.sbev ~n:(S.__expose n) ~kd:(S.__expose kd) ~zr ~zc ~z ?up
      ?work:(Slap_vec.opt_work work) ?w:(Slap_vec.opt_cnt_vec n w)
      ~abr ~abc ab in
  V.__unexpose n 1 w

(** {3 Symmetric-matrix eigenvalue and singular value problems
       (expert & RRR drivers)} *)

(** {4 syevr} *)


type 'n syevr_min_lwork

let syevr_min_lwork n =
  S.__unexpose (I.syevr_min_lwork (S.__expose n))

type 'n syevr_min_liwork

let syevr_min_liwork n =
  S.__unexpose (I.syevr_min_liwork (S.__expose n))

let syevr_opt_lwork ?vectors ?range ?up ?abstol a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  I.syevr_opt_lwork ~n:(S.__expose n) ?vectors ?range ?up ?abstol ~ar ~ac a
  |> Slap_size.unsafe_of_int

let syevr_opt_liwork ?vectors ?range ?up ?abstol a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  I.syevr_opt_liwork ~n:(S.__expose n) ?vectors ?range ?up ?abstol ~ar ~ac a
  |> Slap_size.unsafe_of_int

module type SYEVR_RESULT =
  sig
    type m
    type n
    val value : m Slap_size.t *
                (n, 'cnt) vec *
                (n, m, 'cnt) mat *
                ((m, m) Slap_size.add, 'cnt) Slap_common.int32_vec
  end

let syevr_dyn (type nn) ?vectors ?range ?up ?abstol ?work ?iwork ?w ?z ?isuppz
    (a : (nn, nn, 'a_cd) mat) =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  let syevr k ?zr ?zc ?z ar ac a =
    I.syevr ~n:(S.__expose n) ?vectors ?range ?up ?abstol
      ?work:(Slap_vec.opt_work work) ?iwork:(Slap_vec.opt_work iwork)
      ?w:(Slap_vec.opt_cnt_vec n w)
      ?isuppz:(Slap_vec.opt_cnt_vec (Slap_size.add k k) isuppz)
      ?zr ?zc ?z ~ar ~ac a in
  let res = match z with
    | Some z ->
      let n'', k, zr, zc, z = M.__expose z in
      assert(n = n'');
      let (m, w, z, isuppz) = syevr k ~zr ~zc ~z ar ac a in
      if m > S.__expose k
      then invalid_argf "syevr_dyn: m (= %d) should be smaller than k (= %d)"
          m (S.__expose k) ();
      (S.__unexpose m,
       V.__unexpose n 1 w,
       M.__unexpose n (S.__unexpose m) zr zc z,
       V.__unexpose (S.__unexpose (2 * m)) 1 isuppz)
    | None ->
      let (m, w, z, isuppz) = syevr (S.__unexpose (S.__expose n)) ar ac a in
      (S.__unexpose m,
       V.__unexpose n 1 w,
       M.__unexpose n (S.__unexpose (Array2.dim2 z)) 1 1 z,
       V.__unexpose (S.__unexpose (Array1.dim isuppz)) 1 isuppz) in
  let module R =
    struct
      type n = nn
      type m
      let value = res
    end in
  (module R : SYEVR_RESULT with type n = nn)

(** {4 sygv} *)

let sygv_opt_lwork ?vectors ?up ?itype a b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', n''', br, bc, b = M.__expose b in
  assert(n = n' && n = n'' && n = n''');
  I.sygv_opt_lwork ~n:(S.__expose n) ?vectors ?up ?itype ~ar ~ac a ~br ~bc b
  |> Slap_size.unsafe_of_int

let sygv ?vectors ?up ?work ?w ?itype a b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', n''', br, bc, b = M.__expose b in
  assert(n = n' && n = n'' && n = n''');
  let w = I.sygv ~n:(S.__expose n) ?vectors ?up ?work:(Slap_vec.opt_work work)
                 ?w:(Slap_vec.opt_cnt_vec n w) ?itype ~ar ~ac a ~br ~bc b in
  V.__unexpose n 1 w

(** {4 sbgv} *)

let sbgv ~ka ~kb ?z ?up ?work ?w ab bb =
  let sbsize_a, n, abr, abc, ab = M.__expose ab in
  let sbsize_b, n', bbr, bbc, bb = M.__expose bb in
  assert(n = n');
  assert(sbsize_a = Slap_size.syband_dyn n ka);
  assert(sbsize_b = Slap_size.syband_dyn n kb);
  let zr, zc, z = Slap_mat.opt_mat_alloc prec n n z in
  let w = I.sbgv ~n:(S.__expose n) ~ka:(S.__expose ka) ~kb:(S.__expose kb)
      ~zr ~zc ~z ?up ?work:(Slap_vec.opt_work work)
      ?w:(Slap_vec.opt_cnt_vec n w) ~ar:abr ~ac:abc ab ~br:bbr ~bc:bbc bb in
  V.__unexpose n 1 w
