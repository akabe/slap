(* Sized Linear Algebra Package (SLAP)

   Copyright (C) 2013- Akinori ABE <aabe.65535@gmail.com>

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

let pp_vec ppf x = Io.pp_vec_gen pp_num ppf x

let pp_mat ppf a = Io.pp_mat_gen pp_num ppf a

(* error report functions: *)

let internal_error loc i = raise (Misc.Internal_error (loc, i))

let xxtri_error loc i =
  assert(i <> 0);
  if i > 0
  then Misc.failwithf "%s: singular on index %i" loc i ()
  else internal_error loc i

let sxsv_error loc i =
  assert(i <> 0);
  if i > 0
  then Misc.failwithf "%s: D(%i,%i)=0 in the diagonal pivoting \
                            factorization" loc i i ()
  else internal_error loc i

let pxsv_error loc i =
  assert(i <> 0);
  if i > 0
  then Misc.failwithf "%s: the leading minor of order %i is not positive \
                            definite" loc i ()
  else internal_error "Slap.XSDCZ.posv" i

let lu_error loc i =
  assert(i <> 0);
  if i > 0
  then Misc.failwithf "%s: U(%i,%i)=0 in the LU factorization" loc i i ()
  else internal_error loc i

(** {2 BLAS interface} *)

(** {3 Level 1} *)

(* SWAP *)

external direct_swap :
  n : _ Size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZswap_stub_bc" "lacaml_XSDCZswap_stub"

let swap x y =
  let n, incx, x = Vec.__expose x in
  let n', incy, y = Vec.__expose y in
  assert(n = n');
  direct_swap ~n ~ofsx:1 ~incx ~x ~ofsy:1 ~incy ~y


(* SCAL *)

external direct_scal :
  n : _ Size.t ->
  alpha : 'a ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZscal_stub"

let scal alpha x =
  let n, incx, x = Vec.__expose x in
  direct_scal ~n ~alpha ~ofsx:1 ~incx ~x


(* COPY *)

external direct_copy :
  n : _ Size.t ->
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZcopy_stub_bc" "lacaml_XSDCZcopy_stub"

let copy ?y x =
  let n, incx, x = Vec.__expose x in
  let incy, y = Vec.opt_vec_alloc prec n y in
  direct_copy ~n ~ofsy:1 ~incy ~y ~ofsx:1 ~incx ~x;
  Vec.__unexpose n incy y


(* NRM2 *)

external direct_nrm2 :
  n : _ Size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  float = "lacaml_XSDCZnrm2_stub"

let nrm2 x =
  let n, incx, x = Vec.__expose x in
  direct_nrm2 ~n ~ofsx:1 ~incx ~x


(* AXPY *)

external direct_axpy :
  alpha : 'a ->
  n : _ Size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZaxpy_stub_bc" "lacaml_XSDCZaxpy_stub"

let axpy ?(alpha = one) x y =
  let n, incx, x = Vec.__expose x in
  let n', incy, y = Vec.__expose y in
  assert(n = n');
  direct_axpy ~alpha ~n ~ofsx:1 ~incx ~x ~ofsy:1 ~incy ~y


(* IAMAX/AMAX *)

external direct_iamax :
  n : _ Size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  int = "lacaml_XSDCZiamax_stub"

let iamax x =
  let n, incx, x = Vec.__expose x in
  direct_iamax ~n ~ofsx:1 ~incx ~x

let amax x =
  let n, incx, x = Vec.__expose x in
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
  m : _ Size.t ->
  n : _ Size.t ->
  trans : (_, _, _) Common.trans ->
  alpha : 'a ->
  beta : 'a ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZgemv_stub_bc" "lacaml_XSDCZgemv_stub"

let gemv ?(beta = zero) ?y ~trans ?(alpha = one) a x =
  let am, an, ar, ac, a = Mat.__expose a in
  let n', incx, x = Vec.__expose x in
  let m, n = Common.get_transposed_dim trans am an in
  assert(n = n');
  let incy, y = Vec.opt_vec_alloc prec m y in
  let vy = Vec.__unexpose m incy y in
  if Size.nonzero m && Size.nonzero n
  then direct_gemv ~ofsy:1 ~incy ~y ~ar ~ac ~a ~m:am ~n:an
      ~trans ~alpha ~beta ~ofsx:1 ~incx ~x
  else Vec.fill vy zero;
  vy


(* GBMV *)

external direct_gbmv :
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  m : _ Size.t ->
  n : _ Size.t ->
  kl : _ Size.t ->
  ku : _ Size.t ->
  trans : (_, _, _) Common.trans ->
  alpha : 'a ->
  beta : 'a ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZgbmv_stub_bc" "lacaml_XSDCZgbmv_stub"

let gbmv ~m:am ?(beta = zero) ?y ~trans ?(alpha = one) a kl ku x =
  let gbs, an, ar, ac, a = Mat.__expose a in
  let n', incx, x = Vec.__expose x in
  let m, n = Common.get_transposed_dim trans am an in
  assert(gbs = Size.geband_dyn am an kl ku && n = n');
  let incy, y = Vec.opt_vec_alloc prec m y in
  let vy = Vec.__unexpose m incy y in
  if Size.nonzero m && Size.nonzero n
  then direct_gbmv ~ofsy:1 ~incy ~y ~ar ~ac ~a ~m:am ~n:an
      ~kl ~ku ~trans ~alpha ~beta ~ofsx:1 ~incx ~x
  else Vec.fill vy zero;
  vy


(* SYMV *)

external direct_symv :
  ofsy : int ->
  incy : int ->
  y : ('a, 'b, fortran_layout) Array1.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Size.t ->
  up : [< `U | `L ] Common.uplo ->
  alpha : 'a ->
  beta : 'a ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZsymv_stub_bc" "lacaml_XSDCZsymv_stub"

let symv
    ?(beta = zero)
    ?y
    ?(up = Common.__unexpose_uplo 'U')
    ?(alpha = one)
    a x =
  let n, n', ar, ac, a = Mat.__expose a in
  let n'', incx, x = Vec.__expose x in
  assert(n = n' && n = n'');
  let incy, y = Vec.opt_vec_alloc prec n y in
  let vy = Vec.__unexpose n incy y in
  if Size.nonzero n
  then direct_symv ~ofsy:1 ~incy ~y ~ar ~ac ~a ~n
      ~up ~alpha ~beta ~ofsx:1 ~incx ~x
  else Vec.fill vy zero;
  vy


(* TRMV *)

external direct_trmv :
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Size.t ->
  up : [< `U | `L ] Common.uplo ->
  trans : (_, _, _) Common.trans ->
  diag : Common.diag ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZtrmv_stub_bc" "lacaml_XSDCZtrmv_stub"

let trmv
    ~trans
    ?(diag = Common.non_unit)
    ?(up = Common.__unexpose_uplo 'U')
    a x =
  let n, n', ar, ac, a = Mat.__expose a in
  let n'', incx, x = Vec.__expose x in
  assert(n = n' && n = n'');
  if Size.nonzero n
  then direct_trmv ~ar ~ac ~a ~n ~up ~trans ~diag ~ofsx:1 ~incx ~x


(* TRSV *)

external direct_trsv :
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Size.t ->
  up : [< `U | `L ] Common.uplo ->
  trans : (_, _, _) Common.trans ->
  diag : Common.diag ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZtrsv_stub_bc" "lacaml_XSDCZtrsv_stub"

let trsv
    ~trans
    ?(diag = Common.non_unit)
    ?(up = Common.__unexpose_uplo 'U')
    a x =
  let n, n', ar, ac, a = Mat.__expose a in
  let n'', incx, x = Vec.__expose x in
  assert(n = n' && n = n'');
  if Size.nonzero n
  then direct_trsv ~ar ~ac ~a ~n ~up ~trans ~diag ~ofsx:1 ~incx ~x


(* TPMV *)

external direct_tpmv :
  ofsap : int ->
  ap : ('a, 'b, fortran_layout) Array1.t ->
  n : _ Size.t ->
  up : [< `U | `L ] Common.uplo ->
  trans : (_, _, _) Common.trans ->
  diag : Common.diag ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZtpmv_stub_bc" "lacaml_XSDCZtpmv_stub"

let tpmv
    ~trans
    ?(diag = Common.non_unit)
    ?(up = Common.__unexpose_uplo 'U')
    ap x =
  assert(Vec.check_cnt ap);
  let k, _, ap = Vec.__expose ap in
  let n, incx, x = Vec.__expose x in
  assert(k = Size.packed n);
  if Size.nonzero n
  then direct_tpmv ~ofsap:1 ~ap ~n ~up ~trans ~diag ~ofsx:1 ~incx ~x


(* TPSV *)

external direct_tpsv :
  ofsap : int ->
  ap : ('a, 'b, fortran_layout) Array1.t ->
  n : _ Size.t ->
  up : [< `U | `L ] Common.uplo ->
  trans : (_, _, _) Common.trans ->
  diag : Common.diag ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZtpsv_stub_bc" "lacaml_XSDCZtpsv_stub"

let tpsv
    ~trans
    ?(diag = Common.non_unit)
    ?(up = Common.__unexpose_uplo 'U')
    ap x =
  assert(Vec.check_cnt ap);
  let k, _, ap = Vec.__expose ap in
  let n, incx, x = Vec.__expose x in
  assert(k = Size.packed n);
  if Size.nonzero n
  then direct_tpsv ~ofsap:1 ~ap ~n ~up ~trans ~diag ~ofsx:1 ~incx ~x


(** {3 Level 3} *)

(* GEMM *)

external direct_gemm :
  transa : (_, _, _) Common.trans ->
  transb : (_, _, _) Common.trans ->
  m : _ Size.t ->
  n : _ Size.t ->
  k : _ Size.t ->
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
  let am, ak, ar, ac, a = Mat.__expose a in
  let bk, bn, br, bc, b = Mat.__expose b in
  let m, k = Common.get_transposed_dim transa am ak in
  let k', n = Common.get_transposed_dim transb bk bn in
  assert(k = k');
  let cr, cc, c = Mat.opt_mat_alloc prec m n c in
  let mc = Mat.__unexpose m n cr cc c in
  if Size.nonzero m && Size.nonzero n
  then begin
    if Size.iszero k then Mat.fill mc zero
    else direct_gemm ~transa ~transb ~alpha ~beta
        ~m ~n ~k ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c
  end;
  mc


(* SYMM *)

external direct_symm :
  side : (_, _, _) Common.side ->
  up : [< `U | `L ] Common.uplo ->
  m : _ Size.t ->
  n : _ Size.t->
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
    ?(up = Common.__unexpose_uplo 'U')
    ?(beta = zero)
    ?c
    ?(alpha = one)
    a b =
  let k, k', ar, ac, a = Mat.__expose a in
  let m, n, br, bc, b = Mat.__expose b in
  assert(k = k' && Common.check_side_dim k m n side);
  let cr, cc, c = Mat.opt_mat_alloc prec m n c in
  let mc = Mat.__unexpose m n cr cc c in
  if Size.nonzero m && Size.nonzero n
  then direct_symm ~side ~up ~m ~n ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c ~alpha ~beta
  else Mat.fill mc zero;
  mc


(* TRMM *)

external direct_trmm :
  side : (_, _, _) Common.side ->
  up : [< `U | `L ] Common.uplo ->
  transa : (_, _, _) Common.trans ->
  diag : Common.diag ->
  m : _ Size.t ->
  n : _ Size.t ->
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
    ?(up = Common.__unexpose_uplo 'U')
    ~transa
    ?(diag = Common.non_unit)
    ?(alpha = one)
    ~a b =
  let k, k', ar, ac, a = Mat.__expose a in
  let m, n, br, bc, b = Mat.__expose b in
  assert(k = k' && Common.check_side_dim k m n side);
  if Size.nonzero m && Size.nonzero n
  then direct_trmm ~side ~up ~transa ~diag ~m ~n ~ar ~ac ~a ~br ~bc ~b ~alpha


(* TRSM *)

external direct_trsm :
  side : (_, _, _) Common.side ->
  up : [< `U | `L ] Common.uplo ->
  transa : (_, _, _) Common.trans ->
  diag : Common.diag ->
  m : _ Size.t ->
  n : _ Size.t ->
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
    ?(up = Common.__unexpose_uplo 'U')
    ~transa
    ?(diag = Common.non_unit)
    ?(alpha = one)
    ~a b =
  let k, k', ar, ac, a = Mat.__expose a in
  let m, n, br, bc, b = Mat.__expose b in
  assert(k = k' && Common.check_side_dim k m n side);
  if Size.nonzero m && Size.nonzero n
  then direct_trsm ~side ~up ~transa ~diag ~m ~n ~ar ~ac ~a ~br ~bc ~b ~alpha


(* SYRK *)

external direct_syrk :
  up : [< `U | `L ] Common.uplo ->
  trans : (_, _, _) Common.trans ->
  n : _ Size.t ->
  k : _ Size.t ->
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
    ?(up = Common.__unexpose_uplo 'U')
    ?(beta = zero)
    ?c
    ~trans
    ?(alpha = one)
    a =
  let an, ak, ar, ac, a = Mat.__expose a in
  let n, k = Common.get_transposed_dim trans an ak in
  let cr, cc, c = Mat.opt_mat_alloc prec n n c in
  let mc = Mat.__unexpose n n cr cc c in
  if Size.nonzero k
  then direct_syrk ~up ~trans ~n ~k ~ar ~ac ~a ~cr ~cc ~c ~alpha ~beta
  else Mat.fill mc zero;
  mc


(* SYR2K *)

external direct_syr2k :
  up : [< `U | `L ] Common.uplo ->
  trans : (_, _, _) Common.trans ->
  n : _ Size.t ->
  k : _ Size.t ->
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
    ?(up = Common.__unexpose_uplo 'U')
    ?(beta = zero)
    ?c
    ~trans
    ?(alpha = one)
    a b =
  let am, an, ar, ac, a = Mat.__expose a in
  let bm, bn, br, bc, b = Mat.__expose b in
  assert(am = bm && an = bn);
  let n, k = Common.get_transposed_dim trans am an in
  let cr, cc, c = Mat.opt_mat_alloc prec n n c in
  let mc = Mat.__unexpose n n cr cc c in
  if Size.nonzero k
  then direct_syr2k ~up ~trans ~n ~k ~ar ~ac ~a ~br ~bc ~b ~cr ~cc ~c
      ~alpha ~beta
  else Mat.fill mc zero;
  mc


(** {2 LAPACK interface} *)

(** {3 Auxiliary routines} *)

(* LACPY *)

external direct_lacpy :
  uplo : [< `A | `U | `L ] Common.uplo ->
  m : _ Size.t ->
  n : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  unit = "lacaml_XSDCZlacpy_stub_bc" "lacaml_XSDCZlacpy_stub"

let lacpy ?(uplo = Common.__unexpose_uplo 'A') ?b a =
  let m, n, ar, ac, a = Mat.__expose a in
  let br, bc, b = Mat.opt_mat_alloc prec m n b in
  if Size.nonzero m && Size.nonzero n
  then direct_lacpy ~uplo ~m ~n ~ar ~ac ~a ~br ~bc ~b;
  Mat.__unexpose m n br bc b


(* LASSQ *)

external direct_lassq :
  n : _ Size.t ->
  ofsx : int ->
  incx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  scale : float ->
  sumsq : float ->
  float * float = "lacaml_XSDCZlassq_stub_bc" "lacaml_XSDCZlassq_stub"

let lassq ?(scale = 0.0) ?(sumsq = 1.0) x =
  let n, incx, x = Vec.__expose x in
  direct_lassq ~n ~ofsx:1 ~incx ~x ~scale ~sumsq

(* LARNV *)

external direct_larnv :
  idist : int ->
  iseed : (int32, int32_elt, fortran_layout) Array1.t ->
  n : _ Size.t ->
  ofsx : int ->
  x : ('a, 'b, fortran_layout) Array1.t ->
  unit = "lacaml_XSDCZlarnv_stub"

type larnv_liseed = Size.four

let larnv_iseed_alloc = function
  | None ->
    let iseed = Array1.create int32 fortran_layout 4 in
    Array1.fill iseed 1l;
    iseed
  | Some v ->
    let n, _, iseed = Vec.__expose v in
    assert(Size.__expose n = 4 && Vec.check_cnt v);
    (* Dynamic checks for correctness of elements *)
    let n = Size.__expose n in
    for i = 1 to n do
      if iseed.{i} < 0l || iseed.{i} > 4095l
      then invalid_arg "Slap.XSDCZ.larnv: \
                        `iseed' entries must be between 0 and 4095"
    done;
    if (Int32.to_int iseed.{n}) land 1 = 0
    then invalid_arg "Slap.XSDCZ.larnv: the last `iseed' entry must be odd";
    iseed

let larnv ?(idist = `Normal) ?iseed ~x () =
  assert(Vec.check_cnt x);
  let n, _, x = Vec.__expose x in
  let idist = match idist with
    | `Uniform0 -> 1
    | `Uniform1 -> 2
    | `Normal -> 3 in
  direct_larnv ~idist ~iseed:(larnv_iseed_alloc iseed) ~n ~ofsx:1 ~x;
  Vec.__unexpose n 1 x


(* LANGE *)

type ('m, 'a) lange_min_lwork

let lange_min_lwork m norm =
  let m' = match Common.char_of_norm norm with
    | 'I' -> Size.__expose m
    | _ -> 0 in
  Size.__unexpose m'

external direct_lange :
  norm : _ Common.norm ->
  m : _ Size.t ->
  n : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  work : (float, 'c, fortran_layout) Array1.t ->
  float = "lacaml_XSDCZlange_stub_bc" "lacaml_XSDCZlange_stub"

let lange ?(norm = Common.__unexpose_norm 'O') ?work a =
  let m, n, ar, ac, a = Mat.__expose a in
  let min_lwork = lange_min_lwork m norm in
  let _, work =
    Vec.__alloc_work rprec work
      ~loc:"Slap.XSDCZ.lange" ~min_lwork ~opt_lwork:min_lwork in
  if Size.nonzero m && Size.nonzero n
  then direct_lange ~norm ~m ~n ~ar ~ac ~a ~work
  else 0.0


(* LAUUM *)

external direct_lauum :
  up : [< `U | `L ] Common.uplo ->
  n : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  unit = "lacaml_XSDCZlauum_stub"

let lauum ?(up = Common.__unexpose_uplo 'U') a =
  let n, n', ar, ac, a = Mat.__expose a in
  assert(n = n');
  if Size.nonzero n then direct_lauum ~up ~n ~ar ~ac ~a


(** {3 Linear equations (computational routines)} *)

(* GETRF *)

external direct_getrf :
  m : _ Size.t ->
  n : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  int = "lacaml_XSDCZgetrf_stub_bc" "lacaml_XSDCZgetrf_stub"

let getrf ?ipiv a =
  let m, n, ar, ac, a = Mat.__expose a in
  let k = Size.min m n in
  let ipiv = Vec.opt_cnt_vec_alloc int32 k ipiv in
  let res = Vec.__unexpose k 1 ipiv in
  if Size.nonzero m && Size.nonzero n then begin
    let i = direct_getrf ~m ~n ~ar ~ac ~a ~ipiv in
    if i = 0 then res else lu_error "Slap.XSDCZ.getrf" i
  end else res


(* GETRS *)

external direct_getrs :
  trans : (_, _, _) Common.trans ->
  n : _ Size.t ->
  nrhs : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  int = "lacaml_XSDCZgetrs_stub_bc" "lacaml_XSDCZgetrs_stub"

let getrs ?ipiv ~trans a b =
  let n, n', ar, ac, a = Mat.__expose a in
  let n'', nrhs, br, bc, b = Mat.__expose b in
  assert(n = n' && n = n'');
  if Size.nonzero n && Size.nonzero nrhs then begin
    let ipiv = Vec.opt_cnt_vec_alloc int32 (Size.min n n) ipiv in
    let i = direct_getrs ~trans ~n ~nrhs ~ar ~ac ~a ~br ~bc ~b ~ipiv in
    if i <> 0 then internal_error "Slap.XSDCZ.getrs" i
  end

(* GETRI *)

external direct_getri :
  n : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  int = "lacaml_XSDCZgetri_stub_bc" "lacaml_XSDCZgetri_stub"

type 'n getri_min_lwork = (Size.one, 'n) Size.max

let getri_min_lwork n = Size.max Size.one n

let getri_opt_lwork_aux a =
  let n, n', ar, ac, a = Mat.__expose a in
  assert(n = n');
  let work = Array1.create prec fortran_layout  1 in
  let ipiv = Array1.create int32 fortran_layout 0 in
  let i = direct_getri ~n ~ar ~ac ~a ~ipiv ~work ~lwork:(-1) in
  if i = 0 then Size.__unexpose (int_of_num work.{1})
  else internal_error "Slap.XSDCZ.getri_opt_lwork" i

let getri_opt_lwork a =
  getri_opt_lwork_aux a
  |> Size.__expose
  |> Size.unsafe_of_int

let getri ?ipiv ?work a =
  let n, n', ar, ac, aa = Mat.__expose a in
  assert(n = n');
  if Size.nonzero n then begin
    let lwork, work =
      Vec.__alloc_work prec work ~loc:"Slap.XSDCZ.getri"
        ~min_lwork:(getri_min_lwork n) ~opt_lwork:(getri_opt_lwork_aux a) in
    let ipiv = match ipiv with None -> getrf a | Some ipiv -> ipiv in
    assert(Vec.check_cnt ipiv);
    let k, _, ipiv = Vec.__expose ipiv in
    assert(k = Size.min n n);
    let i = direct_getri ~n ~ar ~ac ~a:aa ~ipiv ~work ~lwork in
    if i <> 0 then xxtri_error "Slap.XSDCZ.getri" i
  end


(* SYTRF  *)

external direct_sytrf :
  up : [< `U | `L ] Common.uplo ->
  n : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  int = "lacaml_XSDCZsytrf_stub_bc" "lacaml_XSDCZsytrf_stub"

type sytrf_min_lwork = Size.one

let sytrf_min_lwork () = Size.one

let sytrf_opt_lwork_aux ~up a =
  let n, n', ar, ac, a = Mat.__expose a in
  assert(n = n');
  let work = Array1.create prec fortran_layout  1 in
  let ipiv = Array1.create int32 fortran_layout 0 in
  let i = direct_sytrf ~up ~n ~ar ~ac ~a ~ipiv ~work ~lwork:(-1) in
  if i = 0 then Size.__unexpose (int_of_num work.{1})
  else failwithf "Slap.XSDCZ.sytrf_opt_lwork: internal error code=%d" i ()

let sytrf_opt_lwork ?(up = Common.__unexpose_uplo 'U') a =
  sytrf_opt_lwork_aux ~up a
  |> Size.__expose
  |> Size.unsafe_of_int

let sytrf ?(up = Common.__unexpose_uplo 'U') ?ipiv ?work a =
  let n, n', ar, ac, aa = Mat.__expose a in
  assert(n = n');
  let ipiv = Vec.opt_cnt_vec_alloc int32 n ipiv in
  if Size.nonzero n then begin
    let lwork, work =
      Vec.__alloc_work prec work ~loc:"Slap.XSDCZ.sytrf"
        ~min_lwork:(sytrf_min_lwork ())
        ~opt_lwork:(sytrf_opt_lwork_aux ~up a) in
    let i = direct_sytrf ~up ~n ~ar ~ac ~a:aa ~ipiv ~work ~lwork in
    if i < 0 then failwithf "Slap.XSDCZ.sytrf: internal error code=%d" i ()
    else if i > 0
    then failwithf "Slap.XSDCZ.sytrf: D(%i,%i)=0 in the factorization" i i ()
  end;
  Vec.__unexpose n 1 ipiv


(* SYTRS *)

external direct_sytrs :
  up : [< `U | `L ] Common.uplo ->
  n : _ Size.t ->
  nrhs : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  int = "lacaml_XSDCZsytrs_stub_bc" "lacaml_XSDCZsytrs_stub"

let sytrs ?(up = Common.__unexpose_uplo 'U') ?ipiv aa bb =
  let n, n', ar, ac, a = Mat.__expose aa in
  let n'', nrhs, br, bc, b = Mat.__expose bb in
  assert(n = n' && n = n'');
  if Size.nonzero n then begin
    let ipiv = match ipiv with None -> sytrf ~up aa | Some ipiv -> ipiv in
    assert(Vec.check_cnt ipiv);
    let n''', _, ipiv = Vec.__expose ipiv in
    assert(n = n''');
    let i = direct_sytrs ~up ~n ~nrhs ~ar ~ac ~a ~br ~bc ~b ~ipiv in
    if i <> 0 then failwithf "Slap.XSDCZ.sytrs: internal error code=%d" i ()
  end


(* SYTRI *)

external direct_sytri :
  up : [< `U | `L ] Common.uplo ->
  n : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  int = "lacaml_XSDCZsytri_stub_bc" "lacaml_XSDCZsytri_stub"

type 'n sytri_min_lwork = 'n

let sytri_min_lwork n = n

let sytri ?(up = Common.__unexpose_uplo 'U') ?ipiv ?work aa =
  let n, n', ar, ac, a = Mat.__expose aa in
  assert(n = n');
  if Size.nonzero n then begin
    let _, work =
      Vec.__alloc_work prec work ~loc:"Slap.XSDCZ.sytri"
        ~min_lwork:(sytri_min_lwork n) ~opt_lwork:(sytri_min_lwork n) in
    let ipiv = match ipiv with None -> sytrf ~up aa | Some ipiv -> ipiv in
    assert(Vec.check_cnt ipiv);
    let n''', _, ipiv = Vec.__expose ipiv in
    assert(n = n''');
    let i = direct_sytri ~up ~n ~ar ~ac ~a ~ipiv ~work in
    if i < 0 then failwithf "Slap.XSDCZ.sytri: internal error code=%d" i ()
    else if i > 0 then failwithf "Slap.XSDCZ.sytri: singular on index %i" i ()
  end


(* POTRF *)

external direct_potrf :
  up : [< `U | `L ] Common.uplo ->
  n : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZpotrf_stub"

let maybe_add_jitter ?jitter a =
  match jitter with
  | None -> ()
  | Some jitter when jitter < zero ->
    invalid_arg "Slap.XSDCZ.potrf: jitter should be non-negative"
  | Some jitter ->
    let d = Mat.diag a in
    ignore (Vec.add_const jitter ~y:d d)

let potrf ?(up = Common.__unexpose_uplo 'U') ?jitter aa =
  let n, n', ar, ac, a = Mat.__expose aa in
  assert(n = n');
  if Size.nonzero n then begin
    maybe_add_jitter ?jitter aa;
    let i = direct_potrf ~up ~n ~ar ~ac ~a in
    if i > 0
    then failwithf "Slap.XSDCZ.potrf: \
                    leading minor of order %d is not positive definite" i ();
    if i < 0 then failwithf "Slap.XSDCZ.potrf: internal error code=%d" i ();
  end


(* POTRS *)

external direct_potrs :
  up : [< `U | `L ] Common.uplo ->
  n : _ Size.t ->
  nrhs : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZpotrs_stub_bc" "lacaml_XSDCZpotrs_stub"

let potrs ?(up = Common.__unexpose_uplo 'U') aa
    ?(factorize = true) ?jitter b =
  let n, n', ar, ac, a = Mat.__expose aa in
  let n'', nrhs, br, bc, b = Mat.__expose b in
  assert(n = n' && n = n'');
  if Size.nonzero n && Size.nonzero nrhs then begin
    if factorize then potrf ~up ?jitter aa;
    let i = direct_potrs ~up ~n ~nrhs ~ar ~ac ~a ~br ~bc ~b in
    if i <> 0 then failwithf "Slap.XSDCZ.potrs: internal error code=%d" i ();
  end


(* POTRI *)

external direct_potri :
  up : [< `U | `L ] Common.uplo ->
  n : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZpotri_stub"

let potri ?(up = Common.__unexpose_uplo 'U')
    ?(factorize = true) ?jitter aa =
  let n, n', ar, ac, a = Mat.__expose aa in
  assert(n = n');
  if Size.nonzero n then begin
    if factorize then potrf ~up ?jitter aa;
    let i = direct_potri ~up ~n ~ar ~ac ~a in
    if i < 0 then failwithf "Slap.XSDCZ.potri: internal error code=%d" i ()
    else if i > 0 then failwithf "Slap.XSDCZ.potri: singular on index %i" i ()
  end


(* TRTRS *)

external direct_trtrs :
  up : [< `U | `L ] Common.uplo ->
  trans : (_, _, _) Common.trans ->
  diag : Common.diag ->
  n : _ Size.t ->
  nrhs : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZtrtrs_stub_bc" "lacaml_XSDCZtrtrs_stub"

let trtrs ?(up = Common.__unexpose_uplo 'U')
    ~trans ?(diag = Common.non_unit) a b =
  let n, n', ar, ac, a = Mat.__expose a in
  let n'', nrhs, br, bc, b = Mat.__expose b in
  assert(n = n' && n = n'');
  if Size.nonzero n && Size.nonzero nrhs then begin
    let i = direct_trtrs ~up ~trans ~diag ~n ~nrhs ~ar ~ac ~a ~br ~bc ~b in
    if i <> 0 then failwithf "Slap.XSDCZ.trtrs: internal error code=%d" i ()
  end


(* TBTRS *)

external direct_tbtrs :
  up : [< `U | `L ] Common.uplo ->
  trans : (_, _, _) Common.trans ->
  diag : Common.diag ->
  n : _ Size.t ->
  kd : _ Size.t ->
  nrhs : _ Size.t ->
  abr : int ->
  abc : int ->
  ab : ('a, 'b, fortran_layout) Array2.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZtbtrs_stub_bc" "lacaml_XSDCZtbtrs_stub"

let tbtrs ~kd ?(up = Common.__unexpose_uplo 'U')
    ~trans ?(diag = Common.non_unit) ab b =
  let sbsize, n, abr, abc, ab = Mat.__expose ab in
  let n', nrhs, br, bc, b = Mat.__expose b in
  assert(n = n' && sbsize = Size.syband_dyn n kd);
  if Size.nonzero n && Size.nonzero nrhs then begin
    let i =
      direct_tbtrs ~up ~trans ~diag ~n ~kd ~nrhs ~abr ~abc ~ab ~br ~bc ~b in
    if i <> 0 then failwithf "Slap.XSDCZ.tbtrs: internal error code=%d" i ()
  end


(* TRTRI *)

external direct_trtri :
  up : [< `U | `L ] Common.uplo ->
  diag : Common.diag ->
  n : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t->
  int = "lacaml_XSDCZtrtri_stub_bc" "lacaml_XSDCZtrtri_stub"

let trtri ?(up = Common.__unexpose_uplo 'U')
    ?(diag = Common.non_unit) a =
  let n, n', ar, ac, a = Mat.__expose a in
  assert(n = n');
  if Size.nonzero n then begin
    let i = direct_trtri ~up ~diag ~n ~ar ~ac ~a in
    if i < 0 then failwithf "Slap.XSDCZ.trtri: internal error code=%d" i ()
    else if i > 0 then failwithf "Slap.XSDCZ.trtri: singular on index %i" i ()
  end


(* GEQRF *)

external direct_geqrf :
  m : _ Size.t ->
  n : _ Size.t ->
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t->
  tau : ('a, 'b, fortran_layout) Array1.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  unit = "lacaml_XSDCZgeqrf_stub_bc" "lacaml_XSDCZgeqrf_stub"

type 'n geqrf_min_lwork = (Size.one, 'n) Size.max

let geqrf_min_lwork ~n = Size.(max one n)

let geqrf_opt_lwork_aux a =
  let m, n, ar, ac, a = Mat.__expose a in
  let work = Array1.create prec fortran_layout 1 in
  direct_geqrf ~m ~n ~ar ~ac ~a ~tau:work ~work ~lwork:(-1);
  Size.__unexpose (int_of_num work.{1})

let geqrf_opt_lwork a =
  geqrf_opt_lwork_aux a
  |> Size.__expose
  |> Size.unsafe_of_int

let geqrf ?work ?tau aa =
  let m, n, ar, ac, a = Mat.__expose aa in
  let k = Size.min m n in
  let tau = Vec.opt_cnt_vec_alloc prec k tau in
  if Size.nonzero m && Size.nonzero n then begin
    let lwork, work =
      Vec.__alloc_work prec work ~loc:"Slap.XSDCZ.geqrf"
        ~min_lwork:(geqrf_min_lwork ~n) ~opt_lwork:(geqrf_opt_lwork_aux aa) in
    direct_geqrf ~m ~n ~ar ~ac ~a ~tau ~work ~lwork
  end;
  Vec.__unexpose k 1 tau

(** {3 Linear equations (simple drivers)} *)

(* GESV *)

external direct_gesv :
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Size.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  nrhs : _ Size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZgesv_stub_bc" "lacaml_XSDCZgesv_stub"

let gesv ?ipiv a b =
  let n, n', ar, ac, a = Mat.__expose a in
  let n'', nrhs, br, bc, b = Mat.__expose b in
  assert(n = n' && n = n'');
  if Size.nonzero n && Size.nonzero nrhs then begin
    let ipiv = Vec.opt_cnt_vec_alloc int32 n ipiv in
    let i = direct_gesv ~ar ~ac ~a ~n ~ipiv ~nrhs ~br ~bc ~b in
    if i <> 0 then lu_error "Slap.XSDCZ.gesv" i
  end


(* GBSV *)

external direct_gbsv :
  abr : int ->
  abc : int ->
  ab : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Size.t ->
  kl : _ Size.t ->
  ku : _ Size.t ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  nrhs : _ Size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZgbsv_stub_bc" "lacaml_XSDCZgbsv_stub"

let gbsv ?ipiv ab kl ku b =
  let lusize, n, abr, abc, ab = Mat.__expose ab in
  let n', nrhs, br, bc, b = Mat.__expose b in
  assert(lusize = Size.luband_dyn n n kl ku && n = n');
  if Size.nonzero n && Size.nonzero nrhs then begin
    let ipiv = Vec.opt_cnt_vec_alloc int32 n ipiv in
    let i = direct_gbsv ~abr ~abc ~ab ~n ~kl ~ku ~ipiv ~nrhs ~br ~bc ~b in
    if i <> 0 then lu_error "Slap.XSDCZ.gbsv" i
  end


(* POSV *)

external direct_posv :
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Size.t ->
  up : [< `U | `L ] Common.uplo ->
  nrhs : _ Size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZposv_stub_bc" "lacaml_XSDCZposv_stub"

let posv ?(up = Common.__unexpose_uplo 'U') a b =
  let n, n', ar, ac, a = Mat.__expose a in
  let n'', nrhs, br, bc, b = Mat.__expose b in
  assert(n = n' && n = n'');
  if Size.nonzero n && Size.nonzero nrhs then begin
    let i = direct_posv ~ar ~ac ~a ~n ~up ~nrhs ~br ~bc ~b in
    if i <> 0 then pxsv_error "Slap.XSDCZ.posv" i
  end


(* PPSV *)

external direct_ppsv :
  ofsap : int ->
  ap : ('a, 'b, fortran_layout) Array1.t ->
  n : _ Size.t ->
  up : [< `U | `L ] Common.uplo ->
  nrhs : _ Size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZppsv_stub_bc" "lacaml_XSDCZppsv_stub"

let ppsv ?(up = Common.__unexpose_uplo 'U') ap b =
  assert(Vec.check_cnt ap);
  let k, _, ap = Vec.__expose ap in
  let n, nrhs, br, bc, b = Mat.__expose b in
  assert(k = Size.packed n);
  if Size.nonzero n && Size.nonzero nrhs then begin
    let i = direct_ppsv ~ofsap:1 ~ap ~n ~up ~nrhs ~br ~bc ~b in
    if i <> 0 then pxsv_error "Slap.XSDCZ.ppsv" i
  end


(* PBSV *)

external direct_pbsv :
  abr : int ->
  abc : int ->
  ab : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Size.t ->
  kd : _ Size.t ->
  up : [< `U | `L ] Common.uplo ->
  nrhs : _ Size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZpbsv_stub_bc" "lacaml_XSDCZpbsv_stub"

let pbsv ?(up = Common.__unexpose_uplo 'U') ~kd ab b =
  let sbsize, n, abr, abc, ab = Mat.__expose ab in
  let n', nrhs, br, bc, b = Mat.__expose b in
  assert(sbsize = Size.syband_dyn n kd && n = n');
  if Size.nonzero n && Size.nonzero nrhs then begin
    let i = direct_pbsv ~abr ~abc ~ab ~n ~kd ~up ~nrhs ~br ~bc ~b in
    if i <> 0 then pxsv_error "Slap.XSDCZ.pbsv" i
  end

(* PTSV *)

external direct_ptsv :
  ofsd : int ->
  d : ('a, 'b, fortran_layout) Array1.t ->
  ofse : int ->
  e : ('a, 'b, fortran_layout) Array1.t ->
  n : _ Size.t ->
  nrhs : _ Size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZptsv_stub_bc" "lacaml_XSDCZptsv_stub"

let ptsv d e b =
  assert(Vec.check_cnt d && Vec.check_cnt e);
  let n, _, d = Vec.__expose d in
  let np, _, e = Vec.__expose e in
  let n', nrhs, br, bc, b = Mat.__expose b in
  assert(n = n' && Size.pred_dyn n = np);
  if Size.nonzero n && Size.nonzero nrhs then begin
    let i = direct_ptsv ~ofsd:1 ~d ~ofse:1 ~e ~n ~nrhs ~br ~bc ~b in
    if i <> 0 then pxsv_error "Slap.XSDCZ.ptsv" i
  end


(* SYSV *)

external direct_sysv :
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  n : _ Size.t ->
  up : [< `U | `L ] Common.uplo ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  nrhs : _ Size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZsysv_stub_bc" "lacaml_XSDCZsysv_stub"

type sysv_min_lwork = Size.one

let sysv_min_lwork () = Size.one

let sysv_opt_lwork_aux ~up a b =
  let n, n', ar, ac, a = Mat.__expose a in
  let n'', nrhs, br, bc, b = Mat.__expose b in
  assert(n = n' && n = n'');
  let work = Array1.create prec fortran_layout 1 in
  let ipiv = Array1.create int32 fortran_layout 0 in
  let i =
    direct_sysv ~ar ~ac ~a ~n ~up ~ipiv ~work ~lwork:(-1) ~nrhs ~br ~bc ~b in
  if i = 0 then Size.__unexpose (int_of_num work.{1})
  else internal_error "Slap.XSDCZ.sysv_opt_lwork" i

let sysv_opt_lwork ?(up = Common.__unexpose_uplo 'U') a b =
  sysv_opt_lwork_aux ~up a b
  |> Size.__expose
  |> Size.unsafe_of_int

let sysv ?(up = Common.__unexpose_uplo 'U') ?ipiv ?work aa bb =
  let n, n', ar, ac, a = Mat.__expose aa in
  let n'', nrhs, br, bc, b = Mat.__expose bb in
  assert(n = n' && n = n'');
  if Size.nonzero n && Size.nonzero nrhs then begin
    let ipiv = Vec.opt_cnt_vec_alloc int32 n ipiv in
    let lwork, work =
      Vec.__alloc_work prec work ~loc:"Slap.XSDCZ.sysv"
        ~min_lwork:(sysv_min_lwork ())
        ~opt_lwork:(sysv_opt_lwork_aux ~up aa bb) in
    let i =
      direct_sysv ~ar ~ac ~a ~n ~up ~ipiv ~work ~lwork ~nrhs ~br ~bc ~b in
    if i <> 0 then sxsv_error "Slap.XSDCZ.sysv" i
  end


(* SPSV *)

external direct_spsv :
  ofsap : int ->
  ap : ('a, 'b, fortran_layout) Array1.t ->
  n : _ Size.t ->
  up : [< `L | `U ] Common.uplo ->
  ipiv : (int32, int32_elt, fortran_layout) Array1.t ->
  nrhs : _ Size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZspsv_stub_bc" "lacaml_XSDCZspsv_stub"

let spsv ?(up = Common.__unexpose_uplo 'U') ?ipiv ap b =
  assert(Vec.check_cnt ap);
  let k, _, ap = Vec.__expose ap in
  let n, nrhs, br, bc, b = Mat.__expose b in
  assert(k = Size.packed n);
  if Size.nonzero n && Size.nonzero nrhs then begin
    let ipiv = Vec.opt_cnt_vec_alloc int32 n ipiv in
    let i = direct_spsv ~ofsap:1 ~ap ~n ~up ~ipiv ~nrhs ~br ~bc ~b in
    if i <> 0 then sxsv_error "Slap.XSDCZ.spsv" i
  end

(** {3 Least squares (simple drivers)} *)

(* GELS *)

external direct_gels :
  ar : int ->
  ac : int ->
  a : ('a, 'b, fortran_layout) Array2.t ->
  m : _ Size.t ->
  n : _ Size.t ->
  trans : (_, _, _) Common.trans ->
  work : ('a, 'b, fortran_layout) Array1.t ->
  lwork : int ->
  nrhs : _ Size.t ->
  br : int ->
  bc : int ->
  b : ('a, 'b, fortran_layout) Array2.t ->
  int = "lacaml_XSDCZgels_stub_bc" "lacaml_XSDCZgels_stub"

type ('m, 'n, 'nrhs) gels_min_lwork =
  (Size.one,
   (('m, 'n) Size.min,
    (('m, 'n) Size.min, 'nrhs) Size.max) Size.add) Size.max

let gels_min_lwork ~m ~n ~nrhs =
  let open Size in
  let min_mn = min m n in
  max one (add min_mn (max min_mn nrhs))

let gels_error loc i =
  assert(i <> 0);
  if i > 0
  then Misc.failwithf
      "%s: failed to converge on off-diagonal element %d" loc i ()
  else internal_error loc i

let gels_opt_lwork_aux ~trans a b =
  let am, an, ar, ac, a = Mat.__expose a in
  let m, nrhs, br, bc, b = Mat.__expose b in
  let m', n = Common.get_transposed_dim trans am an in
  assert(m = m');
  let work = Array1.create prec fortran_layout 1 in
  let i =
    direct_gels ~ar ~ac ~a ~m ~n ~trans ~work ~lwork:(-1) ~nrhs ~br ~bc ~b in
  if i = 0 then Size.__unexpose (int_of_num work.{1})
  else gels_error "Slap.XSDCZ.gels_opt_lwork" i

let gels_opt_lwork ~trans a b =
  gels_opt_lwork_aux ~trans a b
  |> Size.__expose
  |> Size.unsafe_of_int

let gels ?work ~trans aa bb =
  let am, an, ar, ac, a = Mat.__expose aa in
  let m, nrhs, br, bc, b = Mat.__expose bb in
  let m', n = Common.get_transposed_dim trans am an in
  assert(m = m');
  if Size.nonzero m && Size.nonzero n && Size.nonzero nrhs
  then begin
    let lwork, work =
      Vec.__alloc_work prec work ~loc:"Slap.XSDCZ.gels"
        ~min_lwork:(gels_min_lwork ~m ~n ~nrhs)
        ~opt_lwork:(gels_opt_lwork_aux ~trans aa bb) in
    let i = direct_gels ~ar ~ac ~a ~m ~n ~trans ~work ~lwork ~nrhs ~br ~bc ~b in
    if i <> 0 then gels_error "Slap.XSDCZ.gels" i
  end
