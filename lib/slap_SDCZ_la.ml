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

let pp_vec ppf x = Slap_io.pp_vec_gen pp_num ppf x

let pp_mat ppf a = Slap_io.pp_mat_gen pp_num ppf a

(** {2 BLAS interface} *)

(** {3 Level 1} *)

(* SWAP *)

external direct_swap :
  n : _ Slap_size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZswap_stub_bc" "lacaml_XSDCZswap_stub"

let swap x y =
  let n, incx, x = Slap_vec.__expose x in
  let n', incy, y = Slap_vec.__expose y in
  assert(n = n');
  direct_swap ~n ~ofsx:1 ~incx ~x ~ofsy:1 ~incy ~y


(* SCAL *)

external direct_scal :
  n : _ Slap_size.t ->
  alpha : 'a ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZscal_stub"

let scal alpha x =
  let n, incx, x = Slap_vec.__expose x in
  direct_scal ~n ~alpha ~ofsx:1 ~incx ~x


(* COPY *)

external direct_copy :
  n : _ Slap_size.t ->
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZcopy_stub_bc" "lacaml_XSDCZcopy_stub"

let copy ?y x =
  let n, incx, x = Slap_vec.__expose x in
  let incy, y = Slap_vec.opt_vec_alloc prec n y in
  direct_copy ~n ~ofsy:1 ~incy ~y ~ofsx:1 ~incx ~x;
  Slap_vec.__unexpose n incy y


(* NRM2 *)

external direct_nrm2 :
  n : _ Slap_size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  float = "lacaml_XSDCZnrm2_stub"

let nrm2 x =
  let n, incx, x = Slap_vec.__expose x in
  direct_nrm2 ~n ~ofsx:1 ~incx ~x


(* AXPY *)

external direct_axpy :
  alpha : 'a ->
  n : _ Slap_size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZaxpy_stub_bc" "lacaml_XSDCZaxpy_stub"

let axpy ?(alpha = one) x y =
  let n, incx, x = Slap_vec.__expose x in
  let n', incy, y = Slap_vec.__expose y in
  assert(n = n');
  direct_axpy ~alpha ~n ~ofsx:1 ~incx ~x ~ofsy:1 ~incy ~y


(* IAMAX/AMAX *)

external direct_iamax :
  n : _ Slap_size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  int = "lacaml_XSDCZiamax_stub"

let iamax x =
  let n, incx, x = Slap_vec.__expose x in
  direct_iamax ~n ~ofsx:1 ~incx ~x

let amax x =
  let n, incx, x = Slap_vec.__expose x in
  x.{direct_iamax ~n ~ofsx:1 ~incx ~x}


(** {3 Level 2} *)

(* GEMV *)

external direct_gemv :
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  trans : (_, _) Slap_common.trans ->
  alpha : 'a ->
  beta : 'a ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZgemv_stub_bc" "lacaml_XSDCZgemv_stub"

let gemv ?(beta = zero) ?y ~trans ?(alpha = one) a x =
  let am, an, ar, ac, a = Slap_mat.__expose a in
  let n', incx, x = Slap_vec.__expose x in
  let m, n = Slap_common.get_transposed_dim trans am an in
  assert(n = n');
  let incy, y = Slap_vec.opt_vec_alloc prec m y in
  let vy = Slap_vec.__unexpose m incy y in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then direct_gemv ~ofsy:1 ~incy ~y ~ar ~ac ~a ~m:am ~n:an
      ~trans ~alpha ~beta ~ofsx:1 ~incx ~x
  else Slap_vec.fill vy zero;
  vy


(* GBMV *)

external direct_gbmv :
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  kl : _ Slap_size.t ->
  ku : _ Slap_size.t ->
  trans : (_, _) Slap_common.trans ->
  alpha : 'a ->
  beta : 'a ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZgbmv_stub_bc" "lacaml_XSDCZgbmv_stub"

let gbmv ~m:am ?(beta = zero) ?y ~trans ?(alpha = one) a kl ku x =
  let gbs, an, ar, ac, a = Slap_mat.__expose a in
  let n', incx, x = Slap_vec.__expose x in
  let m, n = Slap_common.get_transposed_dim trans am an in
  assert(gbs = Slap_size.geband_dyn am an kl ku && n = n');
  let incy, y = Slap_vec.opt_vec_alloc prec m y in
  let vy = Slap_vec.__unexpose m incy y in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then direct_gbmv ~ofsy:1 ~incy ~y ~ar ~ac ~a ~m:am ~n:an
      ~kl ~ku ~trans ~alpha ~beta ~ofsx:1 ~incx ~x
  else Slap_vec.fill vy zero;
  vy


(* SYMV *)

external direct_symv :
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Slap_size.t ->
  up : [< `U | `L ] Slap_common.uplo ->
  alpha : 'a ->
  beta : 'a ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZsymv_stub_bc" "lacaml_XSDCZsymv_stub"

let symv
    ?(beta = zero)
    ?y
    ?(up = Slap_common.__unexpose_uplo 'U')
    ?(alpha = one)
    a x =
  let n, n', ar, ac, a = Slap_mat.__expose a in
  let n'', incx, x = Slap_vec.__expose x in
  assert(n = n' && n = n'');
  let incy, y = Slap_vec.opt_vec_alloc prec n y in
  let vy = Slap_vec.__unexpose n incy y in
  if Slap_size.nonzero n
  then direct_symv ~ofsy:1 ~incy ~y ~ar ~ac ~a ~n
      ~up ~alpha ~beta ~ofsx:1 ~incx ~x
  else Slap_vec.fill vy zero;
  vy


(* TRMV *)

external direct_trmv :
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Slap_size.t ->
  up : [< `U | `L ] Slap_common.uplo ->
  trans : (_, _) Slap_common.trans ->
  diag : Slap_common.diag ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZtrmv_stub_bc" "lacaml_XSDCZtrmv_stub"

let trmv
    ~trans
    ?(diag = Slap_common.non_unit)
    ?(up = Slap_common.__unexpose_uplo 'U')
    a x =
  let n, n', ar, ac, a = Slap_mat.__expose a in
  let n'', incx, x = Slap_vec.__expose x in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n
  then direct_trmv ~ar ~ac ~a ~n ~up ~trans ~diag ~ofsx:1 ~incx ~x


(* TRSV *)

external direct_trsv :
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Slap_size.t ->
  up : [< `U | `L ] Slap_common.uplo ->
  trans : (_, _) Slap_common.trans ->
  diag : Slap_common.diag ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZtrsv_stub_bc" "lacaml_XSDCZtrsv_stub"

let trsv
    ~trans
    ?(diag = Slap_common.non_unit)
    ?(up = Slap_common.__unexpose_uplo 'U')
    a x =
  let n, n', ar, ac, a = Slap_mat.__expose a in
  let n'', incx, x = Slap_vec.__expose x in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n
  then direct_trsv ~ar ~ac ~a ~n ~up ~trans ~diag ~ofsx:1 ~incx ~x


(* TPMV *)

external direct_tpmv :
  ofsap : int ->
  ap : ('a, 'b, fortran_layout) Array1.t ->
  n : _ Slap_size.t ->
  up : [< `U | `L ] Slap_common.uplo ->
  trans : (_, _) Slap_common.trans ->
  diag : Slap_common.diag ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZtpmv_stub_bc" "lacaml_XSDCZtpmv_stub"

let tpmv
    ~trans
    ?(diag = Slap_common.non_unit)
    ?(up = Slap_common.__unexpose_uplo 'U')
    ap x =
  assert(Slap_vec.check_cnt ap);
  let k, _, ap = Slap_vec.__expose ap in
  let n, incx, x = Slap_vec.__expose x in
  assert(k = Slap_size.packed n);
  if Slap_size.nonzero n
  then direct_tpmv ~ofsap:1 ~ap ~n ~up ~trans ~diag ~ofsx:1 ~incx ~x


(* TPSV *)

external direct_tpsv :
  ofsap : int ->
  ap : ('a, 'b, fortran_layout) Array1.t ->
  n : _ Slap_size.t ->
  up : [< `U | `L ] Slap_common.uplo ->
  trans : (_, _) Slap_common.trans ->
  diag : Slap_common.diag ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZtpsv_stub_bc" "lacaml_XSDCZtpsv_stub"

let tpsv
    ~trans
    ?(diag = Slap_common.non_unit)
    ?(up = Slap_common.__unexpose_uplo 'U')
    ap x =
  assert(Slap_vec.check_cnt ap);
  let k, _, ap = Slap_vec.__expose ap in
  let n, incx, x = Slap_vec.__expose x in
  assert(k = Slap_size.packed n);
  if Slap_size.nonzero n
  then direct_tpsv ~ofsap:1 ~ap ~n ~up ~trans ~diag ~ofsx:1 ~incx ~x


(** {3 Level 3} *)

(* GEMM *)

external direct_gemm :
  transa : (_, _) Slap_common.trans ->
  transb : (_, _) Slap_common.trans ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  k : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  cr : int ->
  cc : int ->
  c : ('a, 'b, fortran_layout) Array2.t ->
  alpha : 'a ->
  beta : 'a ->
  unit = "lacaml_XSDCZgemm_stub_bc" "lacaml_XSDCZgemm_stub"

let gemm ?(beta = zero) ?c ~transa ?(alpha = one) a ~transb b =
  let am, ak, ar, ac, a = Slap_mat.__expose a in
  let bk, bn, br, bc, b = Slap_mat.__expose b in
  let m, k = Slap_common.get_transposed_dim transa am ak in
  let k', n = Slap_common.get_transposed_dim transb bk bn in
  assert(k = k');
  let cr, cc, c = Slap_mat.opt_mat_alloc prec m n c in
  let mc = Slap_mat.__unexpose m n cr cc c in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then begin
    if Slap_size.iszero k then Slap_mat.fill mc zero
    else direct_gemm ~transa ~transb ~alpha ~beta
        ~m ~n ~k ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c
  end;
  mc


(* SYMM *)

external direct_symm :
  side : (_, _, _) Slap_common.side ->
  up : [< `U | `L ] Slap_common.uplo ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  cr : int ->
  cc : int ->
  c : ('a, 'b, fortran_layout) Array2.t ->
  alpha : 'a ->
  beta : 'a ->
  unit = "lacaml_XSDCZsymm_stub_bc" "lacaml_XSDCZsymm_stub"

let symm
    ~side
    ?(up = Slap_common.__unexpose_uplo 'U')
    ?(beta = zero)
    ?c
    ?(alpha = one)
    a b =
  let k, k', ar, ac, a = Slap_mat.__expose a in
  let m, n, br, bc, b = Slap_mat.__expose b in
  assert(k = k' && Slap_common.check_side_dim k m n side);
  let cr, cc, c = Slap_mat.opt_mat_alloc prec m n c in
  let mc = Slap_mat.__unexpose m n cr cc c in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then direct_symm ~side ~up ~m ~n ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c ~alpha ~beta
  else Slap_mat.fill mc zero;
  mc


(* TRMM *)

external direct_trmm :
  side : (_, _, _) Slap_common.side ->
  up : [< `U | `L ] Slap_common.uplo ->
  transa : (_, _) Slap_common.trans ->
  diag : Slap_common.diag ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  alpha : 'a ->
  unit = "lacaml_XSDCZtrmm_stub_bc" "lacaml_XSDCZtrmm_stub"

let trmm
    ~side
    ?(up = Slap_common.__unexpose_uplo 'U')
    ~transa
    ?(diag = Slap_common.non_unit)
    ?(alpha = one)
    ~a b =
  let k, k', ar, ac, a = Slap_mat.__expose a in
  let m, n, br, bc, b = Slap_mat.__expose b in
  assert(k = k' && Slap_common.check_side_dim k m n side);
  if Slap_size.nonzero m && Slap_size.nonzero n
  then direct_trmm ~side ~up ~transa ~diag ~m ~n ~ar ~ac ~a ~br ~bc ~b ~alpha


(* TRSM *)

external direct_trsm :
  side : (_, _, _) Slap_common.side ->
  up : [< `U | `L ] Slap_common.uplo ->
  transa : (_, _) Slap_common.trans ->
  diag : Slap_common.diag ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  alpha : 'a ->
  unit = "lacaml_XSDCZtrsm_stub_bc" "lacaml_XSDCZtrsm_stub"

let trsm
    ~side
    ?(up = Slap_common.__unexpose_uplo 'U')
    ~transa
    ?(diag = Slap_common.non_unit)
    ?(alpha = one)
    ~a b =
  let k, k', ar, ac, a = Slap_mat.__expose a in
  let m, n, br, bc, b = Slap_mat.__expose b in
  assert(k = k' && Slap_common.check_side_dim k m n side);
  if Slap_size.nonzero m && Slap_size.nonzero n
  then direct_trsm ~side ~up ~transa ~diag ~m ~n ~ar ~ac ~a ~br ~bc ~b ~alpha


(* SYRK *)

external direct_syrk :
  up : [< `U | `L ] Slap_common.uplo ->
  trans : (_, _) Slap_common.trans ->
  n : _ Slap_size.t ->
  k : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  cr : int ->
  cc : int ->
  c : ('a, 'b, fortran_layout) Array2.t ->
  alpha : 'a ->
  beta : 'a ->
  unit = "lacaml_XSDCZsyrk_stub_bc" "lacaml_XSDCZsyrk_stub"

let syrk
    ?(up = Slap_common.__unexpose_uplo 'U')
    ?(beta = zero)
    ?c
    ~trans
    ?(alpha = one)
    a =
  let an, ak, ar, ac, a = Slap_mat.__expose a in
  let n, k = Slap_common.get_transposed_dim trans an ak in
  let cr, cc, c = Slap_mat.opt_mat_alloc prec n n c in
  let mc = Slap_mat.__unexpose n n cr cc c in
  if Slap_size.nonzero k
  then direct_syrk ~up ~trans ~n ~k ~ar ~ac ~a ~cr ~cc ~c ~alpha ~beta
  else Slap_mat.fill mc zero;
  mc


(* SYR2K *)

external direct_syr2k :
  up : [< `U | `L ] Slap_common.uplo ->
  trans : (_, _) Slap_common.trans ->
  n : _ Slap_size.t ->
  k : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  cr : int ->
  cc : int ->
  c : ('a, 'b, fortran_layout) Array2.t ->
  alpha : 'a ->
  beta : 'a ->
  unit = "lacaml_XSDCZsyr2k_stub_bc" "lacaml_XSDCZsyr2k_stub"

let syr2k
    ?(up = Slap_common.__unexpose_uplo 'U')
    ?(beta = zero)
    ?c
    ~trans
    ?(alpha = one)
    a b =
  let am, an, ar, ac, a = Slap_mat.__expose a in
  let bm, bn, br, bc, b = Slap_mat.__expose b in
  assert(am = bm && an = bn);
  let n, k = Slap_common.get_transposed_dim trans am an in
  let cr, cc, c = Slap_mat.opt_mat_alloc prec n n c in
  let mc = Slap_mat.__unexpose n n cr cc c in
  if Slap_size.nonzero k
  then direct_syr2k ~up ~trans ~n ~k ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c
      ~alpha ~beta
  else Slap_mat.fill mc zero;
  mc


(** {2 LAPACK interface} *)

(** {3 Auxiliary routines} *)

(* LACPY *)

external direct_lacpy :
  uplo : [< `A | `U | `L ] Slap_common.uplo ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  unit = "lacaml_XSDCZlacpy_stub_bc" "lacaml_XSDCZlacpy_stub"

let lacpy ?(uplo = Slap_common.__unexpose_uplo 'A') ?b a =
  let m, n, ar, ac, a = Slap_mat.__expose a in
  let br, bc, b = Slap_mat.opt_mat_alloc prec m n b in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then direct_lacpy ~uplo ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  Slap_mat.__unexpose m n br bc b


(* LASSQ *)

external direct_lassq :
  n : _ Slap_size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  scale : float ->
  sumsq : float ->
  float * float = "lacaml_XSDCZlassq_stub_bc" "lacaml_XSDCZlassq_stub"

let lassq ?(scale = 0.0) ?(sumsq = 1.0) x =
  let n, incx, x = Slap_vec.__expose x in
  direct_lassq ~n ~ofsx:1 ~incx ~x ~scale ~sumsq

(* LARNV *)

external direct_larnv :
  idist : int ->
  iseed : (int32, int32_elt, fortran_layout) Array1.t ->
  n : _ Slap_size.t ->
  ofsx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZlarnv_stub"

type larnv_liseed = Slap_size.four

let larnv_iseed_alloc = function
  | None ->
    let iseed = Array1.create int32 fortran_layout 4 in
    Array1.fill iseed 1l;
    iseed
  | Some v ->
    let n, _, iseed = Slap_vec.__expose v in
    assert(Slap_size.__expose n = 4 && Slap_vec.check_cnt v);
    (* Dynamic checks for correctness of elements *)
    let n = Slap_size.__expose n in
    for i = 1 to n do
      if iseed.{i} < 0l || iseed.{i} > 4095l
      then invalid_arg "Slap.XSDCZ.larnv: \
                        `iseed' entries must be between 0 and 4095"
    done;
    if (Int32.to_int iseed.{n}) land 1 = 0
    then invalid_arg "Slap.XSDCZ.larnv: the last `iseed' entry must be odd";
    iseed

let larnv ?(idist = `Normal) ?iseed ~x () =
  assert(Slap_vec.check_cnt x);
  let n, _, x = Slap_vec.__expose x in
  let idist = match idist with
    | `Uniform0 -> 1
    | `Uniform1 -> 2
    | `Normal -> 3 in
  direct_larnv ~idist ~iseed:(larnv_iseed_alloc iseed) ~n ~ofsx:1 ~x;
  Slap_vec.__unexpose n 1 x


(* LANGE *)

type ('m, 'a) lange_min_lwork

let lange_min_lwork m norm =
  let m' = match Slap_common.__expose_norm norm with
    | 'I' -> Slap_size.__expose m
    | _ -> 0 in
  Slap_size.__unexpose m'

external direct_lange :
  norm : (_, _) Slap_common.norm ->
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  work : (float, 'c, fortran_layout) Array1.t ->
  float = "lacaml_XSDCZlange_stub_bc" "lacaml_XSDCZlange_stub"

let lange ?(norm = Slap_common.__unexpose_norm 'O') ?work a =
  let m, n, ar, ac, a = Slap_mat.__expose a in
  let min_lwork = lange_min_lwork m norm in
  let _, work =
    Slap_vec.__alloc_work rprec work
      ~loc:"Slap.XSDCZ.lange" ~min_lwork ~opt_lwork:min_lwork in
  if Slap_size.nonzero m && Slap_size.nonzero n
  then direct_lange ~norm ~m ~n ~ar ~ac ~a ~work
  else 0.0


(* LAUUM *)

external direct_lauum :
  up : [< `U | `L ] Slap_common.uplo ->
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  unit = "lacaml_XSDCZlauum_stub"

let lauum ?(up = Slap_common.__unexpose_uplo 'U') a =
  let n, n', ar, ac, a = Slap_mat.__expose a in
  assert(n = n');
  if Slap_size.nonzero n then direct_lauum ~up ~n ~ar ~ac ~a


(** {3 Linear equations (computational routines)} *)

(* GETRF *)

external direct_getrf :
  m : _ Slap_size.t ->
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  int = "lacaml_XSDCZgetrf_stub_bc" "lacaml_XSDCZgetrf_stub"

let getrf ?ipiv a =
  let m, n, ar, ac, a = Slap_mat.__expose a in
  let k = Slap_size.min m n in
  let ipiv = Slap_vec.opt_cnt_vec_alloc int32 k ipiv in
  let res = Slap_vec.__unexpose k 1 ipiv in
  if Slap_size.nonzero m && Slap_size.nonzero n then begin
    let i = direct_getrf ~m ~n ~ar ~ac ~a ~ipiv in
    if i = 0 then res (* success *)
    else if i > 0
    then failwithf "Slap.XSDCZ.getrf: U(%i,%i)=0 in the LU factorization" i i ()
    else failwithf "Slap.XSDCZ.getrf: internal error code=%d" i ()
  end else res


(* GETRS *)

external direct_getrs :
  trans : (_, _) Slap_common.trans ->
  n : _ Slap_size.t ->
  nrhs : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  int = "lacaml_XSDCZgetrs_stub_bc" "lacaml_XSDCZgetrs_stub"

let getrs ?ipiv ~trans a b =
  let n, n', ar, ac, a = Slap_mat.__expose a in
  let n'', nrhs, br, bc, b = Slap_mat.__expose b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs then begin
    let ipiv = Slap_vec.opt_cnt_vec_alloc int32 (Slap_size.min n n) ipiv in
    let i = direct_getrs ~trans ~n ~nrhs ~ar ~ac ~a ~br ~bc ~b ~ipiv in
    if i <> 0 then failwithf "Slap.XSDCZ.getrs: internal error code=%d" i ()
  end

(* GETRI *)

external direct_getri :
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  int = "lacaml_XSDCZgetri_stub_bc" "lacaml_XSDCZgetri_stub"

type 'n getri_min_lwork = (Slap_size.one, 'n) Slap_size.max

let getri_min_lwork n = Slap_size.max Slap_size.one n

let getri_opt_lwork_aux a =
  let n, n', ar, ac, a = Slap_mat.__expose a in
  assert(n = n');
  let work = Array1.create prec fortran_layout  1 in
  let ipiv = Array1.create int32 fortran_layout 0 in
  let i = direct_getri ~n ~ar ~ac ~a ~ipiv ~work ~lwork:(-1) in
  if i = 0 then Slap_size.__unexpose (int_of_num work.{1})
  else failwithf "Slap.XSDCZ.getri_opt_lwork: internal error code=%d" i ()

let getri_opt_lwork a =
  getri_opt_lwork_aux a
  |> Slap_size.__expose
  |> Slap_size.unsafe_of_int

let getri ?ipiv ?work a =
  let n, n', ar, ac, aa = Slap_mat.__expose a in
  assert(n = n');
  if Slap_size.nonzero n then begin
    let lwork, work =
      Slap_vec.__alloc_work prec work ~loc:"Slap.XSDCZ.getri"
        ~min_lwork:(getri_min_lwork n) ~opt_lwork:(getri_opt_lwork_aux a) in
    let ipiv = match ipiv with None -> getrf a | Some ipiv -> ipiv in
    assert(Slap_vec.check_cnt ipiv);
    let k, _, ipiv = Slap_vec.__expose ipiv in
    assert(k = Slap_size.min n n);
    let i = direct_getri ~n ~ar ~ac ~a:aa ~ipiv ~work ~lwork in
    if i < 0 then failwithf "Slap.XSDCZ.getri: internal error code=%d" i ()
    else if i > 0 then failwithf "Slap.XSDCZ.getri: singular on index %i" i ()
  end


(* SYTRF  *)

external direct_sytrf :
  up : [< `U | `L ] Slap_common.uplo ->
  n : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  int = "lacaml_XSDCZsytrf_stub_bc" "lacaml_XSDCZsytrf_stub"

type sytrf_min_lwork = Slap_size.one

let sytrf_min_lwork () = Slap_size.one

let sytrf_opt_lwork_aux ~up a =
  let n, n', ar, ac, a = Slap_mat.__expose a in
  assert(n = n');
  let work = Array1.create prec fortran_layout  1 in
  let ipiv = Array1.create int32 fortran_layout 0 in
  let i = direct_sytrf ~up ~n ~ar ~ac ~a ~ipiv ~work ~lwork:(-1) in
  if i = 0 then Slap_size.__unexpose (int_of_num work.{1})
  else failwithf "Slap.XSDCZ.sytrf_opt_lwork: internal error code=%d" i ()

let sytrf_opt_lwork ?(up = Slap_common.__unexpose_uplo 'U') a =
  sytrf_opt_lwork_aux ~up a
  |> Slap_size.__expose
  |> Slap_size.unsafe_of_int

let sytrf ?(up = Slap_common.__unexpose_uplo 'U') ?ipiv ?work a =
  let n, n', ar, ac, aa = Slap_mat.__expose a in
  assert(n = n');
  let ipiv = Slap_vec.opt_cnt_vec_alloc int32 n ipiv in
  if Slap_size.nonzero n then begin
    let lwork, work =
      Slap_vec.__alloc_work prec work ~loc:"Slap.XSDCZ.sytrf"
        ~min_lwork:(sytrf_min_lwork ())
        ~opt_lwork:(sytrf_opt_lwork_aux ~up a) in
    let i = direct_sytrf ~up ~n ~ar ~ac ~a:aa ~ipiv ~work ~lwork in
    if i < 0 then failwithf "Slap.XSDCZ.sytrf: internal error code=%d" i ()
    else if i > 0
    then failwithf "Slap.XSDCZ.sytrf: D(%i,%i)=0 in the factorization" i i ()
  end;
  Slap_vec.__unexpose n 1 ipiv


(* SYTRS *)

external direct_sytrs :
  up : [< `U | `L ] Slap_common.uplo ->
  n : _ Slap_size.t ->
  nrhs : _ Slap_size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  int = "lacaml_XSDCZsytrs_stub_bc" "lacaml_XSDCZsytrs_stub"

let sytrs ?(up = Slap_common.__unexpose_uplo 'U') ?ipiv aa bb =
  let n, n', ar, ac, a = Slap_mat.__expose aa in
  let n'', nrhs, br, bc, b = Slap_mat.__expose bb in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n then begin
    let ipiv = match ipiv with None -> sytrf ~up aa | Some ipiv -> ipiv in
    assert(Slap_vec.check_cnt ipiv);
    let n''', _, ipiv = Slap_vec.__expose ipiv in
    assert(n = n''');
    let i = direct_sytrs ~up ~n ~nrhs ~ar ~ac ~a ~br ~bc ~b ~ipiv in
    if i <> 0 then failwithf "Slap.XSDCZ.sytrs: internal error code=%d" i ()
  end

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

let trtrs ?up ~trans ?(diag = non_unit) a b =
  let n, n', ar, ac, a = M.__expose a in
  let n'', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && n = n'');
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.trtrs ~n:(S.__expose n) ?up ~trans:(lacaml_trans3 trans)
      ~diag:(lacaml_diag diag) ~ar ~ac a ~nrhs:(S.__expose nrhs) ~br ~bc b

let tbtrs ~kd ?up ~trans ?(diag = non_unit) ab b =
  let sbsize, n, abr, abc, ab = M.__expose ab in
  let n', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && sbsize = Slap_size.syband_dyn n kd);
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.tbtrs ~n:(S.__expose n) ~kd:(S.__expose kd)
      ?up ~trans:(lacaml_trans3 trans)
      ~diag:(lacaml_diag diag) ~abr ~abc ab ~nrhs:(S.__expose nrhs) ~br ~bc b

let trtri ?up ?(diag = non_unit) a =
  let n, n', ar, ac, a = M.__expose a in
  assert(n = n');
  if Slap_size.nonzero n
  then I.trtri ~n:(S.__expose n) ?up ~diag:(lacaml_diag diag) ~ar ~ac a

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
  V.__unexpose k 1 tau

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
  let k, _, ap = V.__expose ap in
  let n, nrhs, br, bc, b = M.__expose b in
  assert(k = Slap_size.packed n);
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.ppsv ~n:(S.__expose n) ?up ~ofsap:1 ap
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
  let n, _, d = V.__expose d in
  let np, _, e = V.__expose e in
  let n', nrhs, br, bc, b = M.__expose b in
  assert(n = n' && Slap_size.pred_dyn n = np);
  if Slap_size.nonzero n && Slap_size.nonzero nrhs
  then I.ptsv ~n:(S.__expose n) ~ofsd:1 d ~ofse:1 e
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
  let k, _, ap = V.__expose ap in
  let n, nrhs, br, bc, b = M.__expose b in
  assert(k = Slap_size.packed n);
  I.spsv ~n:(S.__expose n) ?up ?ipiv:(Slap_vec.opt_cnt_vec n ipiv)
    ~ofsap:1 ap ~nrhs:(S.__expose nrhs) ~br ~bc b

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
