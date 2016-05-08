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

val swap : ('n, 'x_cd) vec -> ('n, 'y_cd) vec -> unit
(** [swap x y] swaps elements in [x] and [y]. *)

val scal : num_type -> ('n, 'cd) vec -> unit
(** [scal c x] multiplies all elements in [x] by scalar value [c],
    and destructively assigns the result to [x].
 *)

val copy : ?y:('n, 'y_cd) vec -> ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [copy ?y x] copies [x] into [y].
    @return vector [y], which is overwritten.
 *)

val nrm2 : ('n, 'cd) vec -> float
(** [nrm2 x] retruns the L2 norm of vector [x]: [||x||]. *)

val axpy : ?alpha:num_type -> ('n, 'x_cd) vec -> ('n, 'y_cd) vec -> unit
(** [axpy ?alpha x y] executes [y := alpha * x + y] with scalar value [alpha],
    and vectors [x] and [y].
    @param alpha default = [1.0]
 *)

val iamax : ('n, 'cd) vec -> int
(** [iamax x] returns the index of the maximum value of all elements in [x]. *)

val amax : ('n, 'cd) vec -> num_type
(** [amax x] finds the maximum value of all elements in [x]. *)

(** {3 Level 2} *)

val gemv :
  ?beta:num_type ->
  ?y:('m, 'y_cd) vec ->
  trans:(('a_m, 'a_n, 'a_cd) mat -> ('m, 'n, 'a_cd) mat) trans3 ->
  ?alpha:num_type ->
  ('a_m, 'a_n, 'a_cd) mat ->
  ('n, 'x_cd) vec -> ('m, 'y_cd) vec
(** [gemv ?beta ?y ~trans ?alpha a x] executes
    [y := alpha * OP(a) * x + beta * y].

    @param beta default = [0.0]
    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap_common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap_common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap_common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param alpha default = [1.0]
 *)

val gbmv :
  m:'a_m Slap_size.t ->
  ?beta:num_type ->
  ?y:('m, 'y_cd) vec ->
  trans:(('a_m, 'a_n, 'a_cd) mat -> ('m, 'n, 'a_cd) mat) trans3 ->
  ?alpha:num_type ->
  (('a_m, 'a_n, 'kl, 'ku) Slap_size.geband, 'a_n, 'a_cd) mat ->
  'kl Slap_size.t ->
  'ku Slap_size.t ->
  ('n, 'x_cd) vec -> ('m, 'y_cd) vec
(** [gbmv ~m ?beta ?y ~trans ?alpha a kl ku x] computes
    [y := alpha * OP(a) * x + beta * y] where [a] is a band matrix stored in
    band storage.
    @return vector [y], which is overwritten.
    @param beta default = [0.0]
    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap_common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap_common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap_common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param alpha default = [1.0]
    @param kl the number of subdiagonals of [a]
    @param ku the number of superdiagonals of [a]
    @since 0.2.0
 *)

val symv :
  ?beta:num_type ->
  ?y:('n, 'y_cd) vec ->
  ?up:[< `L | `U] Slap_common.uplo ->
  ?alpha:num_type ->
  ('n, 'n, 'a_cd) mat ->
  ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [symv ?beta ?y ?up ?alpha a x] executes
    [y := alpha * a * x + beta * y].

    @param beta default = [0.0]
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
    @param alpha default = [1.0]
 *)

val trmv :
  trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
  ?diag:Slap_common.diag ->
  ?up:[< `L | `U] Slap_common.uplo ->
  ('n, 'n, 'a_cd) mat ->
  ('n, 'x_cd) vec -> unit
(** [trmv ~trans ?diag ?up a x] executes [x := OP(a) * x].

    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap_common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap_common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap_common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param diag default = {!Slap_common.non_unit}
      - If [diag] = {!Slap_common.unit}, then [a] is unit triangular;
      - If [diag] = {!Slap_common.non_unit}, then [a] is not unit triangular.
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
 *)

val trsv :
  trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
  ?diag:Slap_common.diag ->
  ?up:[< `L | `U] Slap_common.uplo ->
  ('n, 'n, 'a_cd) mat ->
  ('n, 'b_cd) vec -> unit
(** [trmv ~trans ?diag ?up a b] solves linear system [OP(a) * x = b]
    and destructively assigns [x] to [b].

    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap_common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap_common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap_common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param diag default = {!Slap_common.non_unit}
      - If [diag] = {!Slap_common.unit}, then [a] is unit triangular;
      - If [diag] = {!Slap_common.non_unit}, then [a] is not unit triangular.
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
 *)

val tpmv :
  trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
  ?diag:Slap_common.diag ->
  ?up:[< `L | `U] Slap_common.uplo ->
  ('n Slap_size.packed, cnt) vec ->
  ('n, 'x_cd) vec -> unit
(** [tpmv ~trans ?diag ?up a x] executes [x := OP(a) * x]
    where [a] is a packed triangular matrix.

    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap_common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap_common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap_common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param diag default = {!Slap_common.non_unit}
      - If [diag] = {!Slap_common.unit}, then [a] is unit triangular;
      - If [diag] = {!Slap_common.non_unit}, then [a] is not unit triangular.
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.

    @since 0.2.0
 *)

val tpsv :
  trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
  ?diag:Slap_common.diag ->
  ?up:[< `L | `U] Slap_common.uplo ->
  ('n Slap_size.packed, cnt) vec ->
  ('n, 'x_cd) vec -> unit
(** [tpsv ~trans ?diag ?up a b] solves linear system [OP(a) * x = b]
    and destructively assigns [x] to [b] where [a] is a packed triangular
    matrix.

    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap_common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap_common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap_common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param diag default = {!Slap_common.non_unit}
      - If [diag] = {!Slap_common.unit}, then [a] is unit triangular;
      - If [diag] = {!Slap_common.non_unit}, then [a] is not unit triangular.
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.

    @since 0.2.0
 *)

(** {3 Level 3} *)

val gemm :
  ?beta:num_type ->
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
      - If [transa] = {!Slap_common.normal}, then [OP(a)] = [a];
      - If [transa] = {!Slap_common.trans}, then [OP(a)] = [a^T];
      - If [transa] = {!Slap_common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).
    @param alpha default = [1.0]
    @param transb the transpose flag for [b]:
      - If [transb] = {!Slap_common.normal}, then [OP(b)] = [b];
      - If [transb] = {!Slap_common.trans}, then [OP(b)] = [b^T];
      - If [transb] = {!Slap_common.conjtr}, then [OP(b)] = [b^H]
      (the conjugate transpose of [b]).
 *)

val symm :
  side:('k, 'm, 'n) Slap_common.side ->
  ?up:[< `U | `L ] Slap_common.uplo ->
  ?beta:num_type ->
  ?c:('m, 'n, 'c_cd) mat ->
  ?alpha:num_type ->
  ('k, 'k, 'a_cd) mat ->
  ('m, 'n, 'b_cd) mat -> ('m, 'n, 'c_cd) mat
(** [symm ~side ?up ?beta ?c ?alpha a b] executes

    - [c := alpha * a * b + beta * c] (if [side] = {!Slap_common.left}) or
    - [c := alpha * b * a + beta * c] (if [side] = {!Slap_common.right})

    where [a] is a symmterix matrix, and [b] and [c] are general matrices.

    @param side the side flag to specify direction of multiplication of [a] and
                [b].
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
    @param beta default = [0.0]
    @param alpha default = [1.0]
 *)

val trmm :
  side:('k, 'm, 'n) Slap_common.side ->
  ?up:[< `U | `L ] Slap_common.uplo ->
  transa:(('k, 'k, 'a_cd) mat -> ('k, 'k, 'a_cd) mat) trans3 ->
  ?diag:Slap_common.diag ->
  ?alpha:num_type ->
  a:('k, 'k, 'a_cd) mat ->
  ('m, 'n, 'b_cd) mat -> unit
(** [trmm ~side ?up ~transa ?diag ?alpha ~a b] executes

    - [b := alpha * OP(a) * b] (if [side] = {!Slap_common.left}) or
    - [b := alpha * b * OP(a)] (if [side] = {!Slap_common.right})

    where [a] is a triangular matrix, and [b] is a general matrix.

    @param side the side flag to specify direction of multiplication of [a] and
                [b].
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
    @param transa the transpose flag for [a]:
      - If [transa] = {!Slap_common.normal}, then [OP(a)] = [a];
      - If [transa] = {!Slap_common.trans}, then [OP(a)] = [a^T];
      - If [transa] = {!Slap_common.conjtr}, then [OP(a)] = [a^H]
        (the conjugate transpose of [a]).
    @param diag default = {!Slap_common.non_unit}
      - If [diag] = {!Slap_common.unit}, then [a] is unit triangular;
      - If [diag] = {!Slap_common.non_unit}, then [a] is not unit triangular.
    @param alpha default = [1.0]
 *)

val trsm :
  side:('k, 'm, 'n) Slap_common.side ->
  ?up:[< `U | `L ] Slap_common.uplo ->
  transa:(('k, 'k, 'a_cd) mat -> ('k, 'k, 'a_cd) mat) trans3 ->
  ?diag:Slap_common.diag ->
  ?alpha:num_type ->
  a:('k, 'k, 'a_cd) mat ->
  ('m, 'n, 'b_cd) mat -> unit
(** [trsm ~side ?up ~transa ?diag ?alpha ~a b] solves a system of linear
    equations

    - [OP(a) * x = alpha * b] (if [side] = {!Slap_common.left}) or
    - [x * OP(a) = alpha * b] (if [side] = {!Slap_common.right})

    where [a] is a triangular matrix, and [b] is a general matrix.
    The solution [x] is returned by [b].

    @param side the side flag to specify direction of multiplication of [a] and
                [b].
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
    @param transa the transpose flag for [a]:
      - If [transa] = {!Slap_common.normal}, then [OP(a)] = [a];
      - If [transa] = {!Slap_common.trans}, then [OP(a)] = [a^T];
      - If [transa] = {!Slap_common.conjtr}, then [OP(a)] = [a^H]
        (the conjugate transpose of [a]).
    @param diag default = {!Slap_common.non_unit}
      - If [diag] = {!Slap_common.unit}, then [a] is unit triangular;
      - If [diag] = {!Slap_common.non_unit}, then [a] is not unit triangular.
    @param alpha default = [1.0]
 *)

val syrk :
  ?up:[< `U | `L ] Slap_common.uplo ->
  ?beta:num_type ->
  ?c:('n, 'n, 'c_cd) mat ->
  trans:(('a_n, 'a_k, 'a_cd) mat ->
         ('n, 'k, 'a_cd) mat) Slap_common.trans2 ->
  ?alpha:num_type ->
  ('a_n, 'a_k, 'a_cd) mat -> ('n, 'n, 'c_cd) mat
(** [syrk ?up ?beta ?c ~trans ?alpha a] executes

    - [c := alpha * a * a^T + beta * c] (if [trans] = {!Slap_common.normal}) or
    - [c := alpha * a^T * a + beta * c] (if [trans] = {!Slap_common.trans})

    where [a] is a general matrix and [c] is a symmetric matrix.

    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
    @param beta default = [0.0]
    @param trans the transpose flag for [a]
    @param alpha default = [1.0]
 *)


val syr2k :
  ?up:[< `U | `L ] Slap_common.uplo ->
  ?beta:num_type ->
  ?c:('n, 'n, 'c_cd) mat ->
  trans:(('p, 'q, _) mat ->
         ('n, 'k, _) mat) Slap_common.trans2 ->
  ?alpha:num_type ->
  ('p, 'q, 'a_cd) mat ->
  ('p, 'q, 'b_cd) mat ->
  ('n, 'n, 'c_cd) mat
(** [syr2k ?up ?beta ?c ~trans ?alpha a b] computes

    - [c := alpha * a * b^T + alpha * b * a^T + beta * c]
      (if [trans] = {!Slap_common.normal}) or
    - [c := alpha * a^T * b + alpha * b^T * a + beta * c]
      (if [trans] = {!Slap_common.trans})

    with symmetric matrix [c], and general matrices [a] and [b].

    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
    @param beta default = [0.0]
    @param trans the transpose flag for [a]
    @param alpha default = [1.0]
 *)

(** {2 LAPACK interface} *)

(** {3 Auxiliary routines} *)

val lacpy :
  ?uplo:[< `A | `L | `U ] Slap_common.uplo ->
  ?b:('m, 'n, 'b_cd) mat ->
  ('m, 'n, 'a_cd) mat -> ('m, 'n, 'b_cd) mat
(** [lacpy ?uplo ?b a] copies the matrix [a] into the matrix [b].
    @return [b], which is overwritten.
    @param uplo default = {!Slap_common.upper_lower}
      - If [uplo] = {!Slap_common.upper},
        then the upper triangular part of [a] is copied;
      - If [uplo] = {!Slap_common.lower},
        then the lower triangular part of [a] is copied.
    @param b    default = a fresh matrix.
 *)

val lassq : ?scale:float -> ?sumsq:float -> ('n, 'cd) vec -> float * float
(** [lassq ?scale ?sumsq x]
    @return [(scl, smsq)] where [scl] and [smsq] satisfy
    [scl^2 * smsq = x1^2 + x2^2 + ... + xn^2 + scale^2 * smsq].
    @param scale default = [0.0]
    @param sumsq default = [1.0]
 *)

type larnv_liseed = Slap_size.four

val larnv :
  ?idist:[ `Normal | `Uniform0 | `Uniform1 ] ->
  ?iseed:(larnv_liseed, cnt) Slap_common.int32_vec ->
  x:('n, cnt) vec ->
  unit ->
  ('n, 'cnt) vec
(** [larnv ?idist ?iseed ~x ()] generates a random vector with the random
    distribution specified by [idist] and random seed [iseed].
    @return vector [x], which is overwritten.
    @param idist default = [`Normal]
    @param iseed a four-dimensional integer vector with all ones.
*)

type ('m, 'a) lange_min_lwork

val lange_min_lwork :
  'm Slap_size.t ->
  'a Slap_common.norm4 ->
  ('m, 'a) lange_min_lwork Slap_size.t
(** [lange_min_lwork m norm] computes the minimum length of workspace for
    [lange] routine. [m] is the number of rows in a matrix, and [norm] is
    the sort of matrix norms.
 *)

val lange :
  ?norm:'a Slap_common.norm4 ->
  ?work:('lwork, cnt) rvec ->
  ('m, 'n, 'cd) mat -> float
(** [lange ?norm ?work a]
    @return the norm of matrix [a].
    @param norm default = {!Slap_common.norm_1}.
      - If [norm] = {!Slap_common.norm_1}, the one norm is returned;
      - If [norm] = {!Slap_common.norm_inf}, the infinity norm is returned;
      - If [norm] = {!Slap_common.norm_amax}, the largest absolute value of
        elements in matrix [a] (not a matrix norm) is returned;
      - If [norm] = {!Slap_common.norm_frob}, the Frobenius norm is returned.
    @param work default = an optimum-length vector.
 *)

val lauum : ?up:[< `U | `L ] Slap_common.uplo -> ('n, 'n, 'cd) mat -> unit
(** [lauum ?up a] computes

    - [U * U^T] where [U] is the upper triangular part of matrix [a]
      if [up] is {!Slap_common.upper}.
    - [L^T * L] where [L] is the lower triangular part of matrix [a]
      if [up] is {!Slap_common.lower}.

    The upper or lower triangular part is overwritten.
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
 *)

(** {3 Linear equations (computational routines)} *)

val getrf :
  ?ipiv:(('m, 'n) Slap_size.min, cnt) Slap_common.int32_vec ->
  ('m, 'n, 'cd) mat ->
  (('m, 'n) Slap_size.min, 'cnt) Slap_common.int32_vec
(** [getrf ?ipiv a] computes LU factorization of matrix [a] using partial
    pivoting with row interchanges: [a = P * L * U] where [P] is a permutation
    matrix, and [L] and [U] are lower and upper triangular matrices,
    respectively. the permutation matrix is returned in [ipiv].

    @return vector [ipiv], which is overwritten.

    @raise Failure if the matrix is singular.
 *)

val getrs :
  ?ipiv:(('n, 'n) Slap_size.min, cnt) Slap_common.int32_vec ->
  trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
  ('n, 'n, 'a_cd) mat ->
  ('n, 'n, 'b_cd) mat -> unit
(** [getrs ?ipiv trans a b] solves systems of linear equations [OP(a) * x = b]
    where [a] a ['n]-by-['n] general matrix, each column of matrix [b] is the
    r.h.s. vector, and each column of matrix [x] is the corresponding solution.
    The solution [x] is returned in [b].

    @param ipiv  a result of [gesv] or [getrf]. It is internally computed by
                 [getrf] if omitted.
    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap_common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap_common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap_common.conjtr}, then [OP(a)] = [a^H]
      (the conjugate transpose of [a]).

    @raise Failure if the matrix is singular.
 *)

type 'n getri_min_lwork

val getri_min_lwork : 'n Slap_size.t -> 'n getri_min_lwork Slap_size.t
(** [getri_min_lwork n] computes the minimum length of workspace for [getri]
    routine. [n] is the number of columns or rows in a matrix.
 *)

val getri_opt_lwork : ('n, 'n, 'cd) mat -> (module Slap_size.SIZE)
(** [getri_opt_lwork a] computes the optimal length of workspace for [getri]
    routine.
 *)

val getri :
  ?ipiv:(('n, 'n) Slap_size.min, cnt) Slap_common.int32_vec ->
  ?work:('lwork, cnt) vec ->
  ('n, 'n, 'cd) mat -> unit
(** [getri ?ipiv ?work a] computes the inverse of general matrix [a] by
    LU-factorization. The inverse matrix is returned in [a].

    @param ipiv a result of [gesv] or [getrf]. It is internally computed by
                [getrf] if omitted.
    @param work default = an optimum-length vector.

    @raise Failure if the matrix is singular.
 *)

type sytrf_min_lwork

val sytrf_min_lwork : unit -> sytrf_min_lwork Slap_size.t
(** [sytrf_min_lwork ()] computes the minimum length of workspace for [sytrf]
    routine.
 *)

val sytrf_opt_lwork :
  ?up:bool ->
  ('n, 'n, 'cd) mat -> (module Slap_size.SIZE)
(** [sytrf_opt_lwork ?up a] computes the optimal length of workspace for [sytrf]
    routine.

    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
 *)

val sytrf :
  ?up:bool ->
  ?ipiv:('n, cnt) Slap_common.int32_vec ->
  ?work:('lwork, cnt) vec ->
  ('n, 'n, 'cd) mat -> ('n, 'cnt) Slap_common.int32_vec
(** [sytrf ?up ?ipiv ?work a] factorizes symmetric matrix [a] using the
    Bunch-Kaufman diagonal pivoting method:

    - [a = P * U * D * U^T * P^T] if [up] = [true];
    - [a = P * L * D * L^T * P^T] if [up] = [false]

    where [P] is a permutation matrix, [U] and [L] are upper and lower
    triangular matrices with unit diagonal, and [D] is a symmetric
    block-diagonal matrix. The permutation matrix is returned in [ipiv].

    @return vector [ipiv], which is overwritten.
    @param up default = [true]
    @param work default = an optimum-length vector.

    @raise Failure if [a] is singular.
 *)

val sytrs :
  ?up:bool ->
  ?ipiv:('n, cnt) Slap_common.int32_vec ->
  ('n, 'n, 'a_cd) mat ->
  ('n, 'nrhs, 'b_cd) mat -> unit
(** [sytrs ?up ?ipiv a b] solves systems of linear equations [a * x = b] where
    [a] is a symmetric matrix, each column of matrix [b] is the r.h.s. vector,
    and each column of matrix [x] is the corresponding solution.
    The solution [x] is returned in [b].

    This routine uses the Bunch-Kaufman diagonal pivoting method:

    - [a = P * U * D * U^T * P^T] if [up] = [true];
    - [a = P * L * D * L^T * P^T] if [up] = [false]

    where [P] is a permutation matrix, [U] and [L] are upper and lower
    triangular matrices with unit diagonal, and [D] is a symmetric
    block-diagonal matrix.

    @param up   default = [true]
    @param ipiv a result of [sytrf]. It is internally computed by [sytrf] if
                omitted.

    @raise Failure if [a] is singular.
 *)

type 'n sytri_min_lwork

val sytri_min_lwork : 'n Slap_size.t -> 'n sytri_min_lwork Slap_size.t
(** [sytri_min_lwork ()] computes the minimum length of workspace for [sytri]
    routine.
 *)

val sytri :
  ?up:bool ->
  ?ipiv:('n, cnt) Slap_common.int32_vec ->
  ?work:('lwork, cnt) vec ->
  ('n, 'n, 'cd) mat -> unit
(** [sytri ?up ?ipiv ?work a] computes the inverse of symmetric matrix [a] using
    the Bunch-Kaufman diagonal pivoting method:

    - [a = P * U * D * U^T * P^T] if [up] = [true];
    - [a = P * L * D * L^T * P^T] if [up] = [false]

    where [P] is a permutation matrix, [U] and [L] are upper and lower
    triangular matrices with unit diagonal, and [D] is a symmetric
    block-diagonal matrix.

    @param up   default = [true]
    @param ipiv a result of [sytrf]. It is internally computed by [sytrf] if
                omitted.
    @param work default = an optimum-length vector.

    @raise Failure if [a] is singular.
 *)

val potrf :
  ?up:bool ->
  ?jitter:num_type ->
  ('n, 'n, 'cd) mat -> unit
(** [potrf ?up ?jitter a] computes the Cholesky factorization of symmetrix
    (Hermitian) positive-definite matrix [a]:

    - [a = U^T * U] (real) or [a = U^H * U] (complex) if [up] = [true];
    - [a = L * L^T] (real) or [a = L * L^H] (complex) if [up] = [false]

    where [U] and [L] are upper and lower triangular matrices, respectively.
    Either of them is returned in the upper or lower triangular part of [a],
    as specified by [up].

    @param up default = [true]
    @param jitter default = nothing

    @raise Failure if [a] is singular.
 *)

val potrs :
  ?up:bool ->
  ('n, 'n, 'a_cd) mat ->
  ?factorize:bool ->
  ?jitter:num_type ->
  ('n, 'nrhs, 'b_cd) mat -> unit
(** [potrf ?up a ?jitter b] solves systems of linear equations [a * x = b] using
    the Cholesky factorization of symmetrix (Hermitian) positive-definite matrix
    [a]:

    - [a = U^T * U] (real) or [a = U^H * U] (complex) if [up] = [true];
    - [a = L * L^T] (real) or [a = L * L^H] (complex) if [up] = [false]

    where [U] and [L] are upper and lower triangular matrices, respectively.

    @param up default = [true]
    @param factorize default = [true] ([potrf] is called implicitly)
    @param jitter default = nothing

    @raise Failure if [a] is singular.
 *)

val potri :
  ?up:bool ->
  ?factorize:bool ->
  ?jitter:num_type ->
  ('n, 'n, 'cd) mat -> unit
(** [potrf ?up ?jitter a] computes the inverse of symmetrix (Hermitian)
    positive-definite matrix [a] using the Cholesky factorization:

    - [a = U^T * U] (real) or [a = U^H * U] (complex) if [up] = [true];
    - [a = L * L^T] (real) or [a = L * L^H] (complex) if [up] = [false]

    where [U] and [L] are upper and lower triangular matrices, respectively.

    @param up default = [true]
    @param factorize default = [true] ([potrf] is called implicitly)
    @param jitter default = nothing

    @raise Failure if [a] is singular.
 *)

val trtrs :
  ?up:bool ->
  trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
  ?diag:Slap_common.diag ->
  ('n, 'n, 'a_cd) mat ->
  ('n, 'nrhs, 'b_cd) mat -> unit
(** [trtrs ?up trans ?diag a b] solves systems of linear equations
    [OP(a) * x = b] where [a] is a triangular matrix of order ['n], each column
    of matrix [b] is the r.h.s vector, and each column of matrix [x] is the
    corresponding solution. The solution [x] is returned in [b].

    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param trans the transpose flag for [a]:
      - If [trans] = {!Slap_common.normal}, then [OP(a)] = [a];
      - If [trans] = {!Slap_common.trans}, then [OP(a)] = [a^T];
      - If [trans] = {!Slap_common.conjtr}, then [OP(a)] = [a^H]
        (the conjugate transpose of [a]).
    @param diag default = [`N]
      - If [diag] = [`U], then [a] is unit triangular;
      - If [diag] = [`N], then [a] is not unit triangular.

    @raise Failure if [a] is singular.
 *)

val tbtrs :
  kd:'kd Slap_size.t ->
  ?up:bool ->
  trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans3 ->
  ?diag:Slap_common.diag ->
  (('n, 'kd) Slap_size.syband, 'n, 'a_cd) mat ->
  ('n, 'nrhs, 'b_cd) mat -> unit
(** [tbtrs ~kd ?up ~trans ?diag ab b] solves systems of linear equations
    [OP(A) * x = b] where [A] is a triangular band matrix with [kd] subdiagonals,
    each column of matrix [b] is the r.h.s vector, and each column of matrix [x]
    is the corresponding solution. Matrix [A] is stored into [ab] in band
    storage format. The solution [x] is returned in [b].

    @param kd the number of subdiagonals or superdiagonals in [A].
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [A] is used;
      - If [up] = [false], then the lower triangular part of [A] is used.
    @param trans the transpose flag for [A]:
      - If [trans] = {!Slap_common.normal}, then [OP(A)] = [A];
      - If [trans] = {!Slap_common.trans}, then [OP(A)] = [A^T];
      - If [trans] = {!Slap_common.conjtr}, then [OP(A)] = [A^H]
        (the conjugate transpose of [A]).
    @param diag default = [`N]
      - If [diag] = [`U], then [A] is unit triangular;
      - If [diag] = [`N], then [A] is not unit triangular.

    @raise Failure if the matrix [A] is singular.
    @since 0.2.0
 *)

val trtri : ?up:bool ->
            ?diag:Slap_common.diag ->
            ('n, 'n, 'cd) mat -> unit
(** [trtri ?up ?diag a] computes the inverse of triangular matrix [a]. The
    inverse matrix is returned in [a].

    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param diag default = [`N]
      - If [diag] = [`U], then [a] is unit triangular;
      - If [diag] = [`N], then [a] is not unit triangular.

    @raise Failure if the matrix [a] is singular.
    @since 0.2.0
 *)

type 'n geqrf_min_lwork

val geqrf_min_lwork : n:'n Slap_size.t -> 'n geqrf_min_lwork Slap_size.t
(** [geqrf_min_lwork ~n] computes the minimum length of workspace for [geqrf]
    routine. [n] is the number of columns in a matrix.
 *)

val geqrf_opt_lwork : ('m, 'n, 'cd) mat -> (module Slap_size.SIZE)
(** [geqrf_opt_lwork a] computes the optimum length of workspace for [geqrf]
    routine.
 *)

val geqrf :
  ?work:('lwork, cnt) vec ->
  ?tau:(('m, 'n) Slap_size.min, cnt) vec ->
  ('m, 'n, 'cd) mat -> (('m, 'n) Slap_size.min, 'cnt) vec
(** [geqrf ?work ?tau a] computes the QR factorization of general matrix [a]:
    [a = Q * R] where [Q] is an orthogonal (unitary) matrix and [R] is an
    upper triangular matrix. [R] is returned in [a]. This routine does not
    generate [Q] explicitly. It is generated by [orgqr].

    @return vector [tau], which is overwritten.
    @param work default = an optimum-length vector.
 *)

(** {3 Linear equations (simple drivers)} *)

val gesv :
  ?ipiv:('n, cnt) Slap_common.int32_vec ->
  ('n, 'n, 'a_cd) mat ->
  ('n, 'nrhs, 'b_cd) mat -> unit
(** [gesv ?ipiv a b] solves a system of linear equations [a * x = b] where [a]
    is a ['n]-by-['n] general matrix, each column of matrix [b] is the r.h.s.
    vector, and each column of matrix [x] is the corresponding solution.

    This routine uses LU factorization: [a = P * L * U] with  permutation matrix
    [P], a lower triangular matrix [L] and an upper triangular matrix [U].
    By this function, the upper triangular part of [a] is replaced by [U], the
    lower triangular part by [L], and the solution [x] is returned in [b].

    @raise Failure if the matrix is singular.
    @since 0.2.0
 *)

val gbsv :
  ?ipiv:('n, cnt) Slap_common.int32_vec ->
  (('n, 'n, 'kl, 'ku) Slap_size.luband, 'n, 'a_cd) mat ->
  'kl Slap_size.t -> 'ku Slap_size.t ->
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

    @raise Failure if the matrix is singular.
    @since 0.2.0
 *)

val posv :
  ?up:bool ->
  ('n, 'n, 'a_cd) mat ->
  ('n, 'nrhs, 'b_cd) mat -> unit
(** [posv ?up a b] solves systems of linear equations [a * x = b] where [a] is
    a ['n]-by-['n] symmetric positive-definite matrix, each column of matrix [b]
    is the r.h.s vector, and each column of matrix [x] is the corresponding
    solution. The solution [x] is returned in [b].

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

val ppsv :
  ?up:bool ->
  ('n Slap_size.packed, cnt) vec ->
  ('n, 'nrhs, 'b_cd) mat -> unit
(** [ppsv ?up a b] solves systems of linear equations [a * x = b] where [a] is
    a ['n]-by-['n] symmetric positive-definite matrix stored in packed format,
    each column of matrix [b] is the r.h.s vector, and each column of matrix [x]
    is the corresponding solution. The solution [x] is returned in [b].

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

val pbsv :
  ?up:bool -> kd:'kd Slap_size.t ->
  (('n, 'kd) Slap_size.syband, 'n, 'ab_cd) mat ->
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

val ptsv :
  ('n, cnt) vec ->
  ('n Slap_size.p, cnt) vec ->
  ('n, 'nrhs, 'b_cd) mat -> unit
(** [ptsv d e b] solves systems of linear equations [A * x = b] where [A] is a
    ['n]-by-['n] symmetric positive-definite tridiagonal matrix with diagonal
    elements [d] and subdiagonal elements [e], each column of matrix [b] is the
    r.h.s vector, and each column of matrix [x] is the corresponding solution.
    The solution [x] is returned in [b].

    This routine uses the Cholesky decomposition: [A = L^T * L] (real) or
    [A = L^H * L] (complex) where [L] is a lower triangular matrix.

    @raise Failure if the matrix is singular.
    @since 0.2.0
 *)

val sysv_opt_lwork :
  ?up:bool ->
  ('n, 'n, 'a_cd) mat ->
  ('n, 'nrhs, 'b_cd) mat -> (module Slap_size.SIZE)
(** [sysv_opt_lwork ?up a b] computes the optimal length of workspace for [sysv]
    routine.
 *)

val sysv :
  ?up:bool ->
  ?ipiv:('n, cnt) Slap_common.int32_vec ->
  ?work:('lwork, cnt) vec ->
  ('n, 'n, 'a_cd) mat ->
  ('n, 'nrhs, 'b_cd) mat -> unit
(** [sysv ?up ?ipiv ?work a b] solves systems of linear equations [a * x = b]
    where [a] is a ['n]-by-['n] symmetric matrix, each column of matrix [b] is
    the r.h.s. vector, and each column of matrix [x] is the corresponding
    solution. The solution [x] is returned in [b].

    The diagonal pivoting method is used:
    - If [up] = [true], then [a = U * D * U^T]
    - If [up] = [false], then [a = L * D * L^T]
    where [U] and [L] are the upper and lower triangular matrices, respectively.

    @param up   default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param ipiv a result of [sytrf]. It is internally computed by [sytrf] if
                omitted.
    @param work default = an optimum-length vector.

    @raise Failure if the matrix is singular.
    @since 0.2.0
 *)

val spsv :
  ?up:bool ->
  ?ipiv:('n, cnt) Slap_common.int32_vec ->
  ('n Slap_size.packed, cnt) vec ->
  ('n, 'nrhs, 'b_cd) mat -> unit
(** [spsv ?up a b] solves systems of linear equations [a * x = b] where [a] is
    a ['n]-by-['n] symmetric matrix stored in packed format, each column of
    matrix [b] is the r.h.s. vector, and each column of matrix [x] is the
    corresponding solution. The solution [x] is returned in [b].

    The diagonal pivoting method is used:
    - If [up] = [true], then [a = U * D * U^T]
    - If [up] = [false], then [a = L * D * L^T]
    where [U] and [L] are the upper and lower triangular matrices, respectively.

    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param ipiv a result of [sytrf]. It is internally computed by [sytrf] if
                omitted.

    @raise Failure if the matrix is singular.
    @since 0.2.0
 *)

(** {3 Least squares (simple drivers)} *)

type ('m, 'n, 'nrhs) gels_min_lwork

val gels_min_lwork :
  m:'m Slap_size.t ->
  n:'n Slap_size.t ->
  nrhs:'nrhs Slap_size.t -> ('m, 'n, 'nrhs) gels_min_lwork Slap_size.t
(** [gels_min_lwork ~n] computes the minimum length of workspace for [gels]
    routine.
    @param m    the number of rows in a matrix.
    @param n    the number of columns in a matrix.
    @param nrhs the number of right hand sides.
 *)

val gels_opt_lwork :
  trans:(('am, 'an, 'a_cd) mat -> ('m, 'n, 'a_cd) mat) Slap_common.trans2 ->
  ('am, 'an, 'a_cd) mat ->
  ('m, 'nrhs, 'b_cd) mat -> (module Slap_size.SIZE)
(** [gels_opt_lwork ~trans a b] computes the optimum length of workspace for
    [gels] routine.
 *)

val gels :
  ?work:('work, cnt) vec ->
  trans:(('am, 'an, 'a_cd) mat -> ('m, 'n, 'a_cd) mat) Slap_common.trans2 ->
  ('am, 'an, 'a_cd) mat ->
  ('m, 'nrhs, 'b_cd) mat -> unit
(** [gels ?work ~trans a b] solves an overdetermined or underdetermined system
    of linear equations using QR or LU factorization.

    - If [trans] = {!Slap_common.normal} and ['m >= 'n]: find the least square
      solution to an overdetermined system by minimizing [||b - A * x||^2].
    - If [trans] = {!Slap_common.normal} and ['m < 'n]: find the minimum norm
      solution to an underdetermined system [a * x = b].
    - If [trans] = {!Slap_common.trans}, and ['m >= 'n]: find the minimum norm
      solution to an underdetermined system [a^H * x = b].
    - If [trans] = {!Slap_common.trans} and ['m < 'n]: find the least square
      solution to an overdetermined system by minimizing [||b - A^H * x||^2].

    @param work default = an optimum-length vector.
    @param trans the transpose flag for [a].
 *)
