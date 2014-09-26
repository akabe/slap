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

val pp_num : Format.formatter -> num_type -> unit
(** A pretty-printer for elements in vectors and matrices. *)

val pp_vec : Format.formatter -> ('n, 'cnt_or_dsc) vec -> unit
(** A pretty-printer for column vectors. *)

val pp_mat : Format.formatter -> ('m, 'n, 'cnt_or_dsc) mat -> unit
(** A pretty-printer for matrices. *)

(** {2 BLAS interface} *)

(** {3 Level 1} *)

val swap : x:('n, 'x_cd) vec -> ('n, 'y_cd) vec -> unit
(** [swap ~x y] swaps elements in [x] and [y]. *)

val scal : num_type -> ('n, 'cd) vec -> unit
(** [scal c x] multiplies all elements in [x] by scalar value [c],
    and destructively assigns the result to [x].
 *)

val copy : ?y:('n, 'y_cd) vec -> ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [copy ?y x] copies [x] into [y].
    @return vector [y], which is overwritten.
 *)

val nrm2 : ('n, 'cd) vec -> float
(** [nrm2 x] retruns [||x||]. *)

val axpy : ?alpha:num_type -> x:('n, 'x_cd) vec -> ('n, 'y_cd) vec -> unit
(** [axpy ?alpha ~x y] executes [y := alpha * x + y] with scalar value [alpha],
    and vectors [x] and [y].
    @param alpha default = [1.0]
 *)

val iamax : ('n, 'cd) vec -> int
(** [iamax x] returns the index of the maximum value of all elements in [x]. *)

val amax : ('n, 'cd) vec -> num_type
(** [amax x] finds the maximum value of all elements in [x]. *)

(** {3 Level 2} *)

val gemv : ?beta:num_type ->
           ?y:('m, 'y_cd) vec ->
           trans:(('a_m, 'a_n, 'a_cd) mat -> ('m, 'n, 'a_cd) mat) trans3 ->
           ?alpha:num_type ->
           ('a_m, 'a_n, 'a_cd) mat ->
           ('n, 'x_cd) vec -> ('m, 'y_cd) vec
(** [gemv ?beta ?y ~trans ?alpha a x] executes
    [y := alpha * OP(a) * x + beta * y].

    @param beta default = [0.0]
    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap.Common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap.Common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap.Common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param alpha default = [1.0]
 *)

val gbmv : m:'a_m Size.t ->
           ?beta:num_type ->
           ?y:('m, 'y_cd) vec ->
           trans:(('a_m, 'a_n, 'a_cd) mat -> ('m, 'n, 'a_cd) mat) trans3 ->
           ?alpha:num_type ->
           (('a_m, 'a_n, 'kl, 'ku) Size.geband, 'a_n, 'a_cd) mat ->
           'kl Size.t ->
           'ku Size.t ->
           ('n, 'x_cd) vec -> ('m, 'y_cd) vec
(** [gbmv ~m ?beta ?y ~trans ?alpha a kl ku x] computes
    [y := alpha * OP(a) * x + beta * y] where [a] is a band matrix stored in
    band storage.
    @return vector [y], which is overwritten.
    @param beta default = [0.0]
    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap.Common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap.Common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap.Common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param alpha default = [1.0]
    @param kl the number of subdiagonals of [a]
    @param ku the number of superdiagonals of [a]
    @since 0.2.0
 *)

val symv : ?beta:num_type ->
           ?y:('n, 'y_cd) vec ->
           ?up:bool ->
           ?alpha:num_type ->
           ('n, 'n, 'a_cd) mat ->
           ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [symv ?beta ?y ?up ?alpha a x] executes
    [y := alpha * a * x + beta * y].

    @param beta default = [0.0]
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param alpha default = [1.0]
 *)

val trmv : trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
           ?diag:Common.diag ->
           ?up:bool ->
           ('n, 'n, 'a_cd) mat ->
           ('n, 'x_cd) vec -> unit
(** [trmv ~trans ?diag ?up a x] executes [x := OP(a) * x].

    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap.Common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap.Common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap.Common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param diag default = [`N]
      - If [diag] = [`U], then [a] is unit triangular;
      - If [diag] = [`N], then [a] is not unit triangular.
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
 *)

val trsv : trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
           ?diag:Common.diag ->
           ?up:bool ->
           ('n, 'n, 'a_cd) mat ->
           ('n, 'b_cd) vec -> unit
(** [trmv ~trans ?diag ?up a b] solves linear system [OP(a) * x = b]
    and destructively assigns [x] to [b].

    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap.Common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap.Common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap.Common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param diag default = [`N]
      - If [diag] = [`U], then [a] is unit triangular;
      - If [diag] = [`N], then [a] is not unit triangular.
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
 *)

val tpmv : trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
           ?diag:Common.diag ->
           ?up:bool ->
           ('n Size.packed, cnt) vec ->
           ('n, 'x_cd) vec -> unit
(** [tpmv ~trans ?diag ?up a x] executes [x := OP(a) * x]
    where [a] is a packed triangular matrix.

    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap.Common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap.Common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap.Common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param diag default = [`N]
      - If [diag] = [`U], then [a] is unit triangular;
      - If [diag] = [`N], then [a] is not unit triangular.
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.

    @since 0.2.0
 *)

val tpsv : trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
           ?diag:Common.diag ->
           ?up:bool ->
           ('n Size.packed, cnt) vec ->
           ('n, 'x_cd) vec -> unit
(** [tpsv ~trans ?diag ?up a b] solves linear system [OP(a) * x = b]
    and destructively assigns [x] to [b] where [a] is a packed triangular
    matrix.

    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap.Common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap.Common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap.Common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param diag default = [`N]
      - If [diag] = [`U], then [a] is unit triangular;
      - If [diag] = [`N], then [a] is not unit triangular.
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.

    @since 0.2.0
 *)

(** {3 Level 3} *)

val gemm : ?beta:num_type ->
           ?c:('m, 'n, 'c_cd) mat ->
           transa:(('a_m, 'a_k, 'a_cd) mat -> ('m, 'k, 'a_cd) mat) trans3 ->
           ?alpha:num_type ->
           ('a_m, 'a_k, 'a_cd) mat ->
           transb:(('b_k, 'b_n, 'b_cd) mat -> ('k, 'n, 'b_cd) mat) trans3 ->
           ('b_k, 'b_n, 'b_cd) mat -> ('m, 'n, 'c_cd) mat
(** [gemm ?beta ?c ~transa ?alpha a ~transb b] executes
    [c := alpha * OP(a) * OP(b) + beta * c].

    @param beta default = [0.0]
    @param transa the transpose flag for [a]:
      - If [transa] = {!Slap.Common.normal}, then [OP(a)] = [a];
      - If [transa] = {!Slap.Common.trans}, then [OP(a)] = [a^T];
      - If [transa] = {!Slap.Common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param alpha default = [1.0]
    @param transb the transpose flag for [b]:
      - If [transb] = {!Slap.Common.normal}, then [OP(b)] = [b];
      - If [transb] = {!Slap.Common.trans}, then [OP(b)] = [b^T];
      - If [transb] = {!Slap.Common.conjtr}, then [OP(b)] = [b^H]
      (the conjugate transpose of [b]).
 *)

val symm : side:('k, 'm, 'n) Common.side ->
           ?up:bool ->
           ?beta:num_type ->
           ?c:('m, 'n, 'c_cd) mat ->
           ?alpha:num_type ->
           ('k, 'k, 'a_cd) mat ->
           ('m, 'n, 'b_cd) mat -> ('m, 'n, 'c_cd) mat
(** [symm ~side ?up ?beta ?c ?alpha a b] executes
    [c := alpha * a * b + beta * c] (if [side] = {!Slap.Common.left}) or
    [c := alpha * b * a + beta * c] (if [side] = {!Slap.Common.right})
    where [a] is a symmterix matrix, and [b] and [c] are general matrices.

    @param side the side flag to specify direction of multiplication of [a] and
                [b].
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param beta default = [0.0]
    @param alpha default = [1.0]
 *)

val trmm : side:('k, 'm, 'n) Common.side ->
           ?up:bool ->
           transa:(('k, 'k, 'a_cd) mat -> ('k, 'k, 'a_cd) mat) trans3 ->
           ?diag:Common.diag ->
           ?alpha:num_type ->
           a:('k, 'k, 'a_cd) mat ->
           ('m, 'n, 'b_cd) mat -> unit
(** [trmm ~side ?up ~transa ?diag ?alpha ~a b] executes
    [b := alpha * OP(a) * b] (if [side] = {!Slap.Common.left}) or
    [b := alpha * b * OP(a)] (if [side] = {!Slap.Common.right})
    where [a] is a triangular matrix, and [b] is a general matrix.

    @param side the side flag to specify direction of multiplication of [a] and
                [b].
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param transa the transpose flag for [a]:
      - If [transa] = {!Slap.Common.normal}, then [OP(a)] = [a];
      - If [transa] = {!Slap.Common.trans}, then [OP(a)] = [a^T];
      - If [transa] = {!Slap.Common.conjtr}, then [OP(a)] = [a^H]
        (the conjugate transpose of [a]).
    @param diag default = [`N]
      - If [diag] = [`U], then [a] is unit triangular;
      - If [diag] = [`N], then [a] is not unit triangular.
    @param alpha default = [1.0]
 *)

val trsm : side:('k, 'm, 'n) Common.side ->
           ?up:bool ->
           transa:(('k, 'k, 'a_cd) mat -> ('k, 'k, 'a_cd) mat) trans3 ->
           ?diag:Common.diag ->
           ?alpha:num_type ->
           a:('k, 'k, 'a_cd) mat ->
           ('m, 'n, 'b_cd) mat -> unit
(** [trsm ~side ?up ~transa ?diag ?alpha ~a b] solves linear systems
    [OP(a) * x = alpha * b] (if [side] = {!Slap.Common.left}) or
    [x * OP(a) = alpha * b] (if [side] = {!Slap.Common.right})
    where [a] is a triangular matrix, and [b] is a general matrix.
    The solution [x] is destructively assigned to [b].

    @param side the side flag to specify direction of multiplication of [a] and
                [b].
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param transa the transpose flag for [a]:
      - If [transa] = {!Slap.Common.normal}, then [OP(a)] = [a];
      - If [transa] = {!Slap.Common.trans}, then [OP(a)] = [a^T];
      - If [transa] = {!Slap.Common.conjtr}, then [OP(a)] = [a^H]
        (the conjugate transpose of [a]).
    @param diag default = [`N]
      - If [diag] = [`U], then [a] is unit triangular;
      - If [diag] = [`N], then [a] is not unit triangular.
    @param alpha default = [1.0]
 *)

val syrk : ?up:bool ->
           ?beta:num_type ->
           ?c:('n, 'n, 'c_cd) mat ->
           trans:(('a_n, 'a_k, 'a_cd) mat ->
                  ('n, 'k, 'a_cd) mat) Common.trans2 ->
           ?alpha:num_type ->
           ('a_n, 'a_k, 'a_cd) mat -> ('n, 'n, 'c_cd) mat
(** [syrk ?up ?beta ?c ~trans ?alpha a] executes
    [c := alpha * a * a^T + beta * c] (if [trans] = {!Slap.Common.normal}) or
    [c := alpha * a^T * a + beta * c] (if [trans] = {!Slap.Common.trans})
    where [a] is a general matrix and [c] is a symmetric matrix.

    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param beta default = [0.0]
    @param trans the transpose flag for [a]
    @param alpha default = [1.0]
 *)


val syr2k : ?up:bool ->
            ?beta:num_type ->
            ?c:('n, 'n, 'c_cd) mat ->
            trans:(('n, 'k, _) mat ->
                   ('p, 'q, _) mat) Common.trans2 ->
            ?alpha:num_type ->
            ('p, 'q, 'a_cd) mat ->
            ('p, 'q, 'b_cd) mat ->
            ('n, 'n, 'c_cd) mat
(** [syr2k ?up ?beta ?c ~trans ?alpha a b] computes
    [c := alpha * a * b^T + alpha * b * a^T + beta * c]
    (if [trans] = {!Slap.Common.normal}) or
    [c := alpha * a^T * b + alpha * b^T * a + beta * c]
    (if [trans] = {!Slap.Common.trans})
    with symmetric matrix [c], and general matrices [a] and [b].

    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param beta default = [0.0]
    @param trans the transpose flag for [a]
    @param alpha default = [1.0]
 *)

(** {2 LAPACK interface} *)

(** {3 Auxiliary routines} *)

val lacpy : ?uplo:[ `L | `U ] ->
            ?b:('m, 'n, 'b_cd) mat ->
            ('m, 'n, 'a_cd) mat -> ('m, 'n, 'b_cd) mat
(** [lacpy ?uplo ?b a] copies the matrix [a] into the matrix [b].
    - If [uplo] is omitted, all elements in [a] is copied.
    - If [uplo] is [`U], the upper trapezoidal part of [a] is copied.
    - If [uplo] is [`L], the lower trapezoidal part of [a] is copied.
    @return [b], which is overwritten.
    @param uplo default = all elements in [a] is copied.
    @param b    default = a fresh matrix.
 *)

val lassq : ?scale:float ->
            ?sumsq:float ->
            ('n, 'cd) vec ->
            float * float
(** [lassq ?scale ?sumsq x] see LAPACK documentation.
    @return [(scl, ssq)]
    @param scale default = 0.
    @param sumsq default = 1.
 *)

type larnv_liseed = Size.z Size.s Size.s Size.s Size.s

val larnv : ?idist:[ `Normal | `Uniform0 | `Uniform1 ] ->
            ?iseed:(larnv_liseed, cnt) Common.int32_vec ->
            x:('n, cnt) vec ->
            unit ->
            ('n, 'cnt) vec

type ('m, 'a) lange_min_lwork

val lange_min_lwork : 'm Size.t ->
                      'a Common.norm4 ->
                      ('m, 'a) lange_min_lwork Size.t

val lange : ?norm:'a Common.norm4 ->
            ?work:('lwork, cnt) rvec ->
            ('m, 'n, 'cd) mat -> float
(** [lange ?norm ?work a]
    @return the norm of matrix [a].
    @param norm default = {!Slap.Common.norm_1}.
    @param work default = allocated work space for norm {!Slap.Common.norm_inf}.
 *)

val lauum : ?up:bool -> ('n, 'n, 'cd) mat -> unit
(** [lauum ?up a] computes

    - [U * U^T] where [U] is the upper triangular part of matrix [a]
      if [up] is [true].
    - [L^T * L] where [L] is the lower triangular part of matrix [a]
      if [up] is [false].

    The upper or lower triangular part is overwritten.
    @param up default = [true].
 *)

(** {3 Linear equations (computational routines)} *)

val getrf : ?ipiv:(('m, 'n) Size.min, cnt) Common.int32_vec ->
            ('m, 'n, 'cd) mat ->
            (('m, 'n) Size.min, 'cnt) Common.int32_vec

val getrs : ?ipiv:(('n, 'n) Size.min, cnt) Common.int32_vec ->
            trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
            ('n, 'n, 'a_cd) mat ->
            ('n, 'n, 'b_cd) mat -> unit

type 'n getri_min_lwork

val getri_min_lwork : 'n Size.t -> 'ngetri_min_lwork Size.t

val getri_opt_lwork : ('n, 'n, 'cd) mat -> (module Size.SIZE)

val getri : ?ipiv:(('n, 'n) Size.min, cnt) Common.int32_vec ->
            ?work:('lwork, cnt) vec ->
            ('n, 'n, 'cd) mat -> unit
(** [getri ?ipiv ?work a] computes the inverse of general matrix [a] by
    LU-factorization of [getrf].
    @raise Failure if the matrix is singular.
 *)

type sytrf_min_lwork

val sytrf_min_lwork : unit -> sytrf_min_lwork Size.t

val sytrf_opt_lwork : ?up:bool ->
                      ('n, 'n, 'cd) mat -> (module Size.SIZE)

val sytrf : ?up:bool ->
            ?ipiv:('n, cnt) Common.int32_vec ->
            ?work:('lwork, cnt) vec ->
            ('n, 'n, 'cd) mat -> ('n, 'cnt) Common.int32_vec

val sytrs : ?up:bool ->
            ?ipiv:('n, cnt) Common.int32_vec ->
            ('n, 'n, 'a_cd) mat ->
            ('n, 'nrhs, 'b_cd) mat -> unit

type 'n sytri_min_lwork

val sytri_min_lwork : 'n Size.t -> 'n sytri_min_lwork Size.t

val sytri : ?up:bool ->
            ?ipiv:('n, cnt) Common.int32_vec ->
            ?work:('lwork, cnt) vec ->
            ('n, 'n, 'cd) mat -> unit

val potrf : ?up:bool ->
            ?jitter:num_type ->
            ('n, 'n, 'cd) mat -> unit

val potrs : ?up:bool ->
            ('n, 'n, 'a_cd) mat ->
            ?factorize:bool ->
            ?jitter:num_type ->
            ('n, 'nrhs, 'b_cd) mat -> unit

val potri : ?up:bool ->
            ?factorize:bool ->
            ?jitter:num_type ->
            ('n, 'n, 'cd) mat -> unit

val trtrs : ?up:bool ->
            trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3
            ->
            ?diag:Common.diag ->
            ('n, 'n, 'a_cd) mat ->
            ('n, 'nrhs, 'b_cd) mat -> unit

val trtri : ?up:bool ->
            ?diag:Common.diag ->
            ('n, 'n, 'cd) mat -> unit

type 'n geqrf_min_lwork

val geqrf_min_lwork : n:'n Size.t -> 'n geqrf_min_lwork Size.t

val geqrf_opt_lwork : ('m, 'n, 'cd) mat -> (module Size.SIZE)

val geqrf : ?work:('lwork, cnt) vec ->
            ?tau:(('m, 'n) Size.min, cnt) vec ->
            ('m, 'n, 'cd) mat -> (('m, 'n) Size.min, 'cnt) vec

(** {3 Linear equations (simple drivers)} *)

val gesv : ?ipiv:('n, cnt) Common.int32_vec ->
           ('n, 'n, 'a_cd) mat ->
           ('n, 'nrhs, 'b_cd) mat -> unit

val gbsv : ?ipiv:('n, cnt) Common.int32_vec ->
           (('n, 'n, 'kl, 'ku) Size.luband, 'n, 'a_cd) mat ->
           'kl Size.t -> 'ku Size.t ->
           ('n, 'nrhs, 'b_cd) mat -> unit
(** [gbsv ?ipiv ab kl ku b] solves a system of linear equations [A * X = B]
    where [A] is a ['n]-by-['n] band matrix, each column of matrix [B] is the
    r.h.s. vector, and each column of matrix [X] is the corresponding solution.
    The matrix [A] with [kl] subdiagonals and [ku] superdiagonals is stored into
    [ab] in band storage format for LU factorizaion.

    This routine uses LU factorization: [A = P * L * U] with  permutation matrix
    [P], a lower triangular matrix [L] and an upper triangular matrix [U].
    By this function, the upper triangular part of [A] is replaced by [U], the
    lower triangular part by [L], and the solution [X] is returned in [B].

    @param ipiv A ['n]-dimensional contiguous vector corresponding to [P].
                This is internally computed if omitted.

    @raise Failure if the matrix is singular.
    @since 0.2.0
 *)

val posv : ?up:bool ->
           ('n, 'n, 'a_cd) mat ->
           ('n, 'nrhs, 'b_cd) mat -> unit

val ppsv : ?up:bool ->
           ('n Size.packed, cnt) vec ->
           ('n, 'nrhs, 'b_cd) mat -> unit
(** [ppsv ?up a b] solves systems of linear equations [a * x = b] where [a] is
    a ['n]-by-['n] symmetric positive-definite matrix stored in packed format,
    each column of matrix [b] is the r.h.s vector, and each column of matrix [x]
    is the corresponding solution. Matrix [x] is destructively assigns to [b].

    The Cholesky decomposition is used:
    - If [up] = [true], then [a = U^T * U] (real) or [a = U^H * U] (complex)
    - If [up] = [false], then [a = L^T * L] (real) or [a = L^H * L] (complex)
    where [U] and [L] are the upper and lower triangular matrices, respectively.

    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.

    @raise Failure if the matrix is singular.
    @since 0.2.0
 *)

val pbsv : ?up:bool -> kd:'kd Size.t ->
           (('n, 'kd) Size.syband, 'n, 'ab_cd) mat ->
           ('n, 'nrhs, 'b_cd) mat -> unit
(** [pbsv ?up ~kd ab b] solves systems of linear equations [ab * x = b] where
    [ab] is a ['n]-by-['n] symmetric positive-definite band matrix with [kd]
    subdiangonals, stored in band storage format, each column of matrix [b] is
    the r.h.s vector, and each column of matrix [x] is the corresponding
    solution. The solution [x] is returned in [b].

    This routine uses the Cholesky decomposition:
    - If [up] = [true], then [ab = U^T * U] (real) or [ab = U^H * U] (complex)
    - If [up] = [false], then [ab = L^T * L] (real) or [ab = L^H * L] (complex)
    where [U] and [L] are the upper and lower triangular matrices, respectively.

    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [ab] is used;
      - If [up] = [false], then the lower triangular part of [ab] is used.
    @param kd the number of subdiagonals or superdiagonals in [ab].

    @raise Failure if the matrix is singular.
    @since 0.2.0
 *)

val sysv_opt_lwork : ?up:bool ->
                     ('n, 'n, 'a_cd) mat ->
                     ('n, 'nrhs, 'b_cd) mat -> (module Size.SIZE)

val sysv : ?up:bool ->
           ?ipiv:('n, cnt) Common.int32_vec ->
           ?work:('lwork, cnt) vec ->
           ('n, 'n, 'a_cd) mat ->
           ('n, 'nrhs, 'b_cd) mat -> unit

val spsv : ?up:bool ->
           ?ipiv:('n, cnt) Common.int32_vec ->
           ('n Size.packed, cnt) vec ->
           ('n, 'nrhs, 'b_cd) mat -> unit
(** [spsv ?up a b] solves systems of linear equations [a * x = b] where [a] is
    a ['n]-by-['n] symmetric matrix stored in packed format,
    each column of matrix [b] is the r.h.s vector, and each column of matrix [x]
    is the corresponding solution. Matrix [x] is destructively assigns to [b].

    The diagonal pivoting method is used:
    - If [up] = [true], then [a = U * D * U^T]
    - If [up] = [false], then [a = L * D * L^T]
    where [U] and [L] are the upper and lower triangular matrices, respectively.

    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param ipiv a ['n]-dimensional vector

    @raise Failure if the matrix is singular.
    @since 0.2.0
 *)

(** {3 Least squares (simple drivers)} *)

type ('m, 'n, 'nrhs) gels_min_lwork

val gels_min_lwork : m:'m Size.t ->
                     n:'n Size.t ->
                     nrhs:'nrhs Size.t -> ('m, 'n, 'nrhs) gels_min_lwork Size.t

val gels_opt_lwork : trans:(('am, 'an, 'a_cd) mat ->
                            ('m, 'n, 'a_cd) mat) Common.trans2 ->
                     ('m, 'n, 'a_cd) mat ->
                     ('m, 'nrhs, 'b_cd) mat -> (module Size.SIZE)

val gels : ?work:('work, cnt) vec ->
           trans:(('am, 'an, 'a_cd) mat ->
                  ('m, 'n, 'a_cd) mat) Common.trans2 ->
           ('m, 'n, 'a_cd) mat ->
           ('m, 'nrhs, 'b_cd) mat -> unit
(** [gels ?work ~trans a b] see LAPACK documentation.
    @param work default = an optimum-length vector.
 *)
