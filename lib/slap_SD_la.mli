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

val dot : ('n, 'x_cd) vec -> ('n, 'y_cd) vec -> float
(** [dot x y]
    @return the inner product of the vectors [x] and [y].
 *)

val asum : ('n, 'x_cd) vec -> float
(** [asum x]
    @return the sum of absolute values of elements in the vector [x].
 *)

(** {3 Level 2} *)

val sbmv :
  k:'k Slap_size.t ->
  ?y:('n, 'y_cd) vec ->
  (('n, 'k) Slap_size.syband, 'n, 'a_cd) mat ->
  ?up:[< `U | `L ] Slap_common.uplo ->
  ?alpha:float ->
  ?beta:float ->
  ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [sbmv ~k ?y a ?up ?alpha ?beta x] computes [y := alpha * a * x + beta * y]
    where [a] is a ['n]-by-['n] symmetric band matrix with [k]
    super-(or sub-)diagonals, and [x] and [y] are ['n]-dimensional vectors.
    @return vector [y], which is overwritten.
    @param k the number of superdiangonals or subdiangonals
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
    @param alpha default = [1.0]
    @param beta default = [0.0]
    @since 0.2.0
 *)

val ger :
  ?alpha:float ->
  ('m, 'x_cd) vec ->
  ('n, 'y_cd) vec ->
  ('m, 'n, 'a_cd) mat -> ('m, 'n, 'a_cd) mat
(** [ger ?alpha x y a] computes [a := alpha * x * y^T + a] with
    the general matrix [a], the vector [x] and
    the transposed vector [y^T] of [y].
    @return matrix [a], which is overwritten.
    @param alpha default = [1.0]
 *)

val syr :
  ?alpha:float ->
  ?up:[< `U | `L ] Slap_common.uplo ->
  ('n, 'x_cd) vec ->
  ('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat
(** [syr ?alpha x a] computes [a := alpha * x * x^T + a] with
    the symmetric matrix [a], the vector [x] and
    the transposed vector [x^T] of [x].
    @return matrix [a], which is overwritten.
    @param alpha default = [1.0]
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
 *)

(** {2 LAPACK interface} *)

(** {3 Auxiliary routines} *)

(** {4 lansy} *)

type ('m, 'a) lansy_min_lwork

val lansy_min_lwork :
  'n Slap_size.t ->
  'a Slap_common.norm4 ->
  ('n, 'a) lansy_min_lwork Slap_size.t
(** [lansy_min_lwork n norm] computes the minimum length of workspace for
    [lansy] routine. [n] is the number of rows or columns in a matrix.
    [norm] is a matrix norm.
 *)

val lansy :
  ?up:[< `U | `L ] Slap_common.uplo ->
  ?norm:'norm Slap_common.norm4 ->
  ?work:('lwork, cnt) vec ->
  ('n, 'n, 'cd) mat -> float
(** [lansy ?up ?norm ?work a]
    @return the norm of matrix [a].
    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
    @param norm default = {!Slap_common.norm_1}
      - If [norm] = {!Slap_common.norm_1}, the one norm is returned;
      - If [norm] = {!Slap_common.norm_inf}, the infinity norm is returned;
      - If [norm] = {!Slap_common.norm_amax}, the largest absolute value of
        elements in matrix [a] (not a matrix norm) is returned;
      - If [norm] = {!Slap_common.norm_frob}, the Frobenius norm is returned.
    @param work default = an optimum-length vector.
 *)

(** {4 lamch} *)

val lamch : [ `B | `E | `L | `M | `N | `O | `P | `R | `S | `U ] -> float
(** [lamch cmach] see LAPACK documentation. *)

(** {3 Linear equations (computational routines)} *)

(** {4 orgqr} *)

type 'n orgqr_min_lwork

val orgqr_min_lwork : n:'n Slap_size.t -> 'n orgqr_min_lwork Slap_size.t
(** [orgqr_min_lwork ~n] computes the minimum length of workspace for
    [orgqr] routine. [n] is the number of columns in a matrix.
 *)

val orgqr_opt_lwork : tau:('k, cnt) vec ->
                      ('m, 'n, 'cd) mat -> (module Slap_size.SIZE)
(** [orgqr_min_lwork ~tau a] computes the optimum length of workspace for
    [orgqr] routine.
 *)

val orgqr_dyn : ?work:('lwork, cnt) vec ->
                tau:('k, cnt) vec ->
                ('m, 'n, 'cd) mat -> unit
(** [orgqr_dyn ?work ~tau a] generates the orthogonal matrix [Q] of the QR
    factorization formed by [geqrf]/[geqpf].

    @param work default = an optimum-length vector
    @param tau  a result of [geqrf]

    @raise Invalid_argument if the following inequality is not satisfied:
                            [(Mat.dim1 a) >= (Mat.dim2 a) >= (Vec.dim tau)],
                            i.e., ['m >= 'n >= 'k].
 *)

(** {4 ormqr} *)

type ('r, 'm, 'n) ormqr_min_lwork

val ormqr_min_lwork : side:('r, 'm, 'n) Slap_common.side ->
                      m:'m Slap_size.t ->
                      n:'n Slap_size.t ->
                      ('r, 'm, 'n) ormqr_min_lwork Slap_size.t
(** [ormqr_min_lwork ~side ~m ~n] computes the minimum length of workspace for
    [ormqr] routine.

    @param side the side flag to specify direction of matrix multiplication.
    @param m    the number of rows in a matrix.
    @param n    the number of columns in a matrix.
 *)

val ormqr_opt_lwork : side:('r, 'm, 'n) Slap_common.side ->
                      trans:(('r, 'r, _) mat -> ('r, 'r, _) mat) Slap_common.trans2 ->
                      tau:('k, cnt) vec ->
                      ('r, 'k, 'a_cd) mat ->
                      ('m, 'n, 'c_cd) mat -> (module Slap_size.SIZE)
(** [ormqr_opt_lwork ~side ~trans ~tau a c] computes the optimum length of
    workspace for [ormqr] routine.

    @param side  the side flag to specify direction of matrix multiplication.
    @param trans the transpose flag for orthogonal matrix [Q].
    @param tau   a result of [geqrf].
 *)

val ormqr_dyn : side:('r, 'm, 'n) Slap_common.side ->
                trans:(('r, 'r, _) mat -> ('r, 'r, _) mat) Slap_common.trans2 ->
                ?work:('lwork, cnt) vec ->
                tau:('k, cnt) vec ->
                ('r, 'k, 'a_cd) mat ->
                ('m, 'n, 'c_cd) mat -> unit
(** [ormqr_dyn ~side ~trans ?work ~tau a c] multiplies a matrix [c] by the
    orthogonal matrix [Q] of the QR factorization formed by [geqrf]/[geqpf]:

    - [Q * c] if [side] = {!Slap_common.left} and
                 [trans] = {!Slap_common.normal};
    - [Q^T * c] if [side] = {!Slap_common.left} and
                   [trans] = {!Slap_common.trans};
    - [c * Q] if [side] = {!Slap_common.right} and
                 [trans] = {!Slap_common.normal};
    - [c * Q^T] if [side] = {!Slap_common.right} and
                   [trans] = {!Slap_common.trans}.

    @param side  the side flag to specify direction of matrix multiplication of
                 [Q] and [c].
    @param trans the transpose flag for orthogonal matrix [Q].
    @param work  default = an optimum-length vector.
    @param tau   a result of [geqrf].

    @raise Invalid_argument if the following inequality is not satisfied:
                            - ['m >= 'k] if [side] = {!Slap_common.left};
                            - ['n >= 'k] if [side] = {!Slap_common.right}.
 *)

(** {4 gecon} *)

type 'n gecon_min_lwork

val gecon_min_lwork : 'n Slap_size.t -> 'n gecon_min_lwork Slap_size.t
(** [gecon_min_lwork n] computes the minimum length of workspace [work] for
    [gecon] routine. [n] is the number of rows or columns in a matrix.
 *)

type 'n gecon_min_liwork

val gecon_min_liwork : 'n Slap_size.t -> 'n gecon_min_liwork Slap_size.t
(** [gecon_min_liwork n] computes the minimum length of workspace [iwork] for
    [gecon] routine. [n] is the number of rows or columns in a matrix.
 *)

val gecon : ?norm:_ Slap_common.norm2 ->
            ?anorm:float ->
            ?work:('lwork, cnt) vec ->
            ?iwork:('liwork, cnt) Slap_common.int32_vec ->
            ('n, 'n, 'cd) mat -> float
(** [gecon ?norm ?anorm ?work ?iwork a] estimates the reciprocal of the
    condition number of general matrix [a].

    @param norm  default = {!Slap_common.norm_1}.
      - If [norm] = {!Slap_common.norm_1}, the one norm is returned;
      - If [norm] = {!Slap_common.norm_inf}, the infinity norm is returned.
    @param anorm default = the norm of matrix [a] as returned by [lange].
    @param work  default = an optimum-length vector.
    @param iwork default = an optimum-length vector.
 *)

(** {4 sycon} *)

type 'n sycon_min_lwork

val sycon_min_lwork : 'n Slap_size.t -> 'n sycon_min_lwork Slap_size.t
(** [sycon_min_lwork n] computes the minimum length of workspace [work] for
    [sycon] routine. [n] is the number of rows or columns in a matrix.
 *)

type 'n sycon_min_liwork

val sycon_min_liwork : 'n Slap_size.t -> 'n sycon_min_liwork Slap_size.t
(** [sycon_min_liwork n] computes the minimum length of workspace [iwork] for
    [sycon] routine. [n] is the number of rows or columns in a matrix.
 *)

val sycon :
  ?up:[< `U | `L ] Slap_common.uplo ->
  ?ipiv:('n, cnt) Slap_common.int32_vec ->
  ?anorm:float ->
  ?work:('lwork, cnt) vec ->
  ?iwork:('liwork, cnt) Slap_common.int32_vec ->
  ('n, 'n, 'cd) mat -> float
(** [sycon ?up ?ipiv ?anorm ?work ?iwork a] estimates the reciprocal of the
    condition number of symmetric matrix [a]. Since [a] is symmetric, the
    1-norm is equal to the infinity norm.

    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
    @param ipiv a result of [sytrf]. It is internally computed by [sytrf] if
                omitted.
    @param anorm default = the norm of matrix [a] as returned by [lange].
    @param work  default = an optimum-length vector.
    @param iwork default = an optimum-length vector.
 *)

(** {4 pocon} *)

type 'n pocon_min_lwork

val pocon_min_lwork : 'n Slap_size.t -> 'n pocon_min_lwork Slap_size.t
(** [pocon_min_lwork n] computes the minimum length of workspace [work] for
    [pocon] routine. [n] is the number of rows or columns in a matrix.
 *)

type 'n pocon_min_liwork

val pocon_min_liwork : 'n Slap_size.t -> 'n pocon_min_liwork Slap_size.t
(** [pocon_min_liwork n] computes the minimum length of workspace [iwork] for
    [pocon] routine. [n] is the number of rows or columns in a matrix.
 *)

val pocon :
  ?up:[< `U | `L ] Slap_common.uplo ->
  ?anorm:float ->
  ?work:('lwork, cnt) vec ->
  ?iwork:('liwork, cnt) Slap_common.int32_vec ->
  ('n, 'n, 'cd) mat -> float
(** [pocon ?up ?anorm ?work ?iwork a] estimates the reciprocal of the
    condition number of symmetric positive-definite matrix [a].
    Since [a] is symmetric, the 1-norm is equal to the infinity norm.

    @param up default = {!Slap_common.upper}
      - If [up] = {!Slap_common.upper},
        then the upper triangular part of [a] is used;
      - If [up] = {!Slap_common.lower},
        then the lower triangular part of [a] is used.
    @param anorm default = the norm of matrix [a] as returned by [lange].
    @param work  default = an optimum-length vector.
    @param iwork default = an optimum-length vector.
 *)

(** {3 Least squares (expert drivers)} *)

(** {4 gelsy} *)

type ('m, 'n, 'nrhs) gelsy_min_lwork

val gelsy_min_lwork : m:'m Slap_size.t ->
                      n:'n Slap_size.t ->
                      nrhs:'nrhs Slap_size.t ->
                      ('m, 'n, 'nrhs) gelsy_min_lwork Slap_size.t
(** [gelsy_min_lwork ~m ~n ~nrhs] computes the minimum length of workspace for
    [gelsy] routine.

    @param m    the number of rows in a matrix.
    @param n    the number of columns in a matrix.
    @param nrhs the number of right hand sides.
 *)

val gelsy_opt_lwork : ('m, 'n, 'a_cd) mat ->
                      ('n, 'nrhs, 'b_cd) mat -> (module Slap_size.SIZE)
(** [gelsy_opt_lwork a b] computes the optimum length of workspace for
    [gelsy] routine.
 *)

val gelsy : ('m, 'n, 'a_cd) mat ->
            ?rcond:float ->
            ?jpvt:('n, cnt) Slap_common.int32_vec ->
            ?work:('lwork, cnt) vec ->
            ('n, 'nrhs, 'b_cd) mat -> int
(** [gelsy a ?rcond ?jpvt ?work b] computes the minimum-norm solution to a
    linear least square problem (minimize [||b - a * x||]) using a complete
    orthogonal factorization of [a].

    @return the effective rank of [a].
    @param rcond default = [-1.0] (machine precision)
    @param jpvt  default = a ['n]-dimensional vector.
    @param work  default = an optimum-length vector.
 *)

(** {4 gelsd} *)

type ('m, 'n, 'nrhs) gelsd_min_lwork

val gelsd_min_lwork : m:'m Slap_size.t ->
                      n:'n Slap_size.t ->
                      nrhs:'nrhs Slap_size.t ->
                      ('m, 'n, 'nrhs) gelsd_min_lwork Slap_size.t
(** [gelsd_min_lwork ~m ~n ~nrhs] computes the minimum length of workspace for
    [gelsd] routine.

    @param m    the number of rows in a matrix.
    @param n    the number of columns in a matrix.
    @param nrhs the number of right hand sides.
 *)

val gelsd_opt_lwork : ('m, 'n, 'a_cd) mat ->
                      ('n, 'nrhs, 'b_cd) mat -> (module Slap_size.SIZE)
(** [gelsd_opt_lwork a b] computes the optimum length of workspace for
    [gelsd] routine.
 *)

type ('m, 'n, 'nrhs) gelsd_min_iwork

val gelsd_min_iwork : 'm Slap_size.t ->
                      'n Slap_size.t ->
                      ('m, 'n, 'nrhs) gelsd_min_iwork Slap_size.t
(** [gelsd_min_iwork ~m ~n ~nrhs] computes the minimum length of workspace
    [iwork] for [gelsd] routine.

    @param m    the number of rows in a matrix.
    @param n    the number of columns in a matrix.
 *)

val gelsd : ('m, 'n, 'a_cd) mat ->
            ?rcond:float ->
            ?s:(('m, 'n) Slap_size.min, cnt) vec ->
            ?work:('lwork, cnt) vec ->
            ?iwork:('liwork, cnt) vec ->
            ('n, 'nrhs, 'b_cd) mat -> int
(** [gelsd a ?rcond ?jpvt ?work b] computes the minimum-norm solution to a
    linear least square problem (minimize [||b - a * x||]) using the singular
    value decomposition (SVD) of [a] and a divide and conquer method.

    @return the effective rank of [a].
    @param rcond default = [-1.0] (machine precision)
    @param s     the singular values of [a] in decreasing order.
                 They are implicitly computed if omitted.
    @param work  default = an optimum-length vector.
    @param iwork default = an optimum-length vector.

    @raise Failure if the function fails to converge.
 *)

(** {4 gelss} *)

type ('m, 'n, 'nrhs) gelss_min_lwork

val gelss_min_lwork : m:'m Slap_size.t ->
                      n:'n Slap_size.t ->
                      nrhs:'nrhs Slap_size.t ->
                      ('m, 'n, 'nrhs) gelss_min_lwork Slap_size.t
(** [gelss_min_lwork ~m ~n ~nrhs] computes the minimum length of workspace for
    [gelss] routine.

    @param m    the number of rows in a matrix.
    @param n    the number of columns in a matrix.
    @param nrhs the number of right hand sides.
 *)

val gelss_opt_lwork : ('m, 'n, 'a_cd) mat ->
                      ('n, 'nrhs, 'b_cd) mat -> (module Slap_size.SIZE)
(** [gelss_min_lwork a b] computes the optimum length of workspace for
    [gelss] routine.
 *)

val gelss : ('m, 'n, 'a_cd) mat ->
            ?rcond:float ->
            ?s:(('m, 'n) Slap_size.min, cnt) vec ->
            ?work:('lwork, cnt) vec ->
            ('n, 'nrhs, 'b_cd) mat -> int
(** [gelss a ?rcond ?work b] computes the minimum-norm solution to a
    linear least square problem (minimize [||b - a * x||]) using the singular
    value decomposition (SVD) of [a].

    @return the effective rank of [a].
    @param rcond default = [-1.0] (machine precision)
    @param s     the singular values of [a] in decreasing order.
                 They are implicitly computed if omitted.
    @param work  default = an optimum-length vector.

    @raise Failure if the function fails to converge.
 *)

(** {3 General SVD routines} *)

(** {4 gesvd} *)

type ('m, 'n) gesvd_min_lwork

val gesvd_min_lwork : m:'m Slap_size.t -> n:'n Slap_size.t ->
                      ('m, 'n) gesvd_min_lwork Slap_size.t
(** [gesvd_min_lwork ~m ~n] computes the minimum length of workspace for
    [gesvd] routine.

    @param m the number of rows in a matrix.
    @param n the number of columns in a matrix.
 *)

val gesvd_opt_lwork : jobu:('u_cols, 'm, ('m, 'n) Slap_size.min,
                            Slap_size.z, Slap_size.z) Slap_common.svd_job ->
                      jobvt:('vt_rows, 'n, ('m, 'n) Slap_size.min,
                             Slap_size.z, Slap_size.z) Slap_common.svd_job ->
                      ?s:(('m, 'n) Slap_size.min, cnt) vec ->
                      ?u:('m, 'u_cols, 'u_cd) mat ->
                      ?vt:('vt_rows, 'n, 'vt_cd) mat ->
                      ('m, 'n, 'a_cd) mat -> (module Slap_size.SIZE)
(** [gesvd_opt_lwork ~jobu ~jobvt ?s ?u ?vt] computes the optimum length of
    workspace for [gesvd] routine.

    @param jobu  the SVD job flag for [u].
    @param jobvt the SVD job flag for [vt].
    @param s     a return location for singular values.
    @param u     a return location for left singular vectors.
    @param vt    a return location for (transposed) right singular vectors.
 *)

val gesvd : jobu:('u_cols, 'm, ('m, 'n) Slap_size.min,
                  Slap_size.z, Slap_size.z) Slap_common.svd_job ->
            jobvt:('vt_rows, 'n, ('m, 'n) Slap_size.min,
                   Slap_size.z, Slap_size.z) Slap_common.svd_job ->
            ?s:(('m, 'n) Slap_size.min, cnt) vec ->
            ?u:('m, 'u_cols, 'u_cd) mat ->
            ?vt:('vt_rows, 'n, 'vt_cd) mat ->
            ?work:('lwork, cnt) vec ->
            ('m, 'n, 'a_cd) mat ->
            (('m, 'n) Slap_size.min, 'cnt) vec
            * ('m, 'u_cols, 'cnt) mat
            * ('vt_rows, 'n, 'cnt) mat
(** [gesvd ?jobu ?jobvt ?s ?u ?vt ?work a] computes the singular value
    decomposition (SVD) of ['m]-by-['n] general rectangular matrix [a]:
    [a = U * D * V^T] where

    - [D] is an ['m]-by-['n] matrix (the diagonal elements in [D] are singular
      values in descreasing order, and other elements are zeros),
    - [U] is an ['m]-by-['m] orthogonal matrix (the columns in [U] are left
      singular vectors), and
    - [V] is an ['n]-by-['n] orthogonal matrix (the columns in [V] are right
      singular vectors).

    @return [(s, u, vt)] with singular values [s] in descreasing order,
            left singular vectors [u] and right singular vectors [vt].
    @param jobu the SVD job flag for [u]:
      - If [jobu] = {!Slap_common.svd_all}, then all ['m] columns of [U] are
        returned in [u]. (['u_cols] = ['m].)
      - If [jobu] = {!Slap_common.svd_top}, then the first [('m, 'n) min]
        columns of [U] are returned in [u]. (['u_cols] = [('m, 'n) min].)
      - If [jobu] = {!Slap_common.svd_overwrite}, then the first [('m, 'n) min]
        columns of [U] are overwritten on [a]. (['u_cols] = [z] since [u]
        is unused.)
      - If [jobu] = {!Slap_common.svd_no}, then no columns of [U] are computed.
        (['u_cols] = [z].)
    @param jobvt the SVD job flag for [vt]:
      - If [jobvt] = {!Slap_common.svd_all}, then all ['n] rows of [V^T] are
        returned in [vt]. (['vt_rows] = ['n].)
      - If [jobvt] = {!Slap_common.svd_top}, then the first [('m, 'n) min]
        rows of [V^T] are returned in [vt]. (['vt_rows] = [('m, 'n) min].)
      - If [jobvt] = {!Slap_common.svd_overwrite}, then the first [('m, 'n) min]
        rows of [V^T] are overwritten on [a]. (['vt_cols] = [z] since [vt]
        is unused.)
      - If [jobvt] = {!Slap_common.svd_no}, then no columns of [V^T] are
        computed. (['vt_cols] = [z].)
    @param s    a return location for singular values.
                (default = an implicitly allocated vector.)
    @param u    a return location for left singular vectors.
                (default = an implicitly allocated matrix.)
    @param vt   a return location for (transposed) right singular vectors.
                (default = an implicitly allocated matrix.)
    @param work default = an optimum-length vector.

    ({b Note}: [jobu] and [jobvt] cannot both be {!Slap_common.svd_overwrite}.)
 *)

(** {4 gesdd} *)

type ('m, 'n) gesdd_liwork

val gesdd_liwork : m:'m Slap_size.t -> n:'n Slap_size.t -> ('m, 'n) gesdd_liwork Slap_size.t
(** [gesdd_liwork ~m ~n] computes the length of workspace [iwork] for
    [gesdd] routine.

    @param m the number of rows in a matrix.
    @param n the number of columns in a matrix.
 *)

type ('m, 'n, 'jobz) gesdd_min_lwork

val gesdd_min_lwork : jobz:('u_cols * 'vt_rows,
                            'm * 'n,
                            ('m, 'n) Slap_size.min * ('m, 'n) Slap_size.min,
                            'm * 'n,
                            Slap_size.z * Slap_size.z) Slap_common.svd_job ->
                      m:'m Slap_size.t -> n:'n Slap_size.t -> unit ->
                      ('m, 'n, 'u_cols * 'vt_rows) gesdd_min_lwork Slap_size.t
(** [gesdd_min_lwork ~m ~n] computes the minimum length of workspace [work] for
    [gesdd] routine.

    @param jobz the SVD job flag.
    @param m    the number of rows in a matrix.
    @param n    the number of columns in a matrix.
 *)

val gesdd_opt_lwork : jobz:('u_cols * 'vt_rows,
                            'm * 'n,
                            ('m, 'n) Slap_size.min * ('m, 'n) Slap_size.min,
                            'm * 'n,
                            Slap_size.z * Slap_size.z) Slap_common.svd_job ->
                      ?s:(('m, 'n) Slap_size.min, cnt) vec ->
                      ?u:('m, 'u_cols, 'u_cd) mat ->
                      ?vt:('vt_rows, 'n, 'vt_cd) mat ->
                      ?iwork:('liwork, cnt) Slap_common.int32_vec ->
                      ('m, 'n, 'a_cd) mat -> (module Slap_size.SIZE)
(** [gesdd_opt_lwork ~jobz ?s ?u ?vt ?iwork a] computes the optimum length of
    workspace [work] for [gesdd] routine.

    @param jobz the SVD job flag.
    @param s     a return location for singular values.
                 (default = an implicitly allocated vector.)
    @param u     a return location for left singular vectors.
                 (default = an implicitly allocated matrix.)
    @param vt    a return location for (transposed) right singular vectors.
                 (default = an implicitly allocated matrix.)
    @param iwork default = an optimum-length vector.
 *)

val gesdd : jobz:('u_cols * 'vt_rows,
                  'm * 'n,
                  ('m, 'n) Slap_size.min * ('m, 'n) Slap_size.min,
                  'm * 'n,
                  Slap_size.z * Slap_size.z) Slap_common.svd_job ->
            ?s:(('m, 'n) Slap_size.min, cnt) vec ->
            ?u:('m, 'u_cols, 'u_cd) mat ->
            ?vt:('vt_rows, 'n, 'vt_cd) mat ->
            ?work:('lwork, cnt) vec ->
            ?iwork:('liwork, cnt) Slap_common.int32_vec ->
            ('m, 'n, 'a_cd) mat ->
            (('m, 'n) Slap_size.min, 'cnt) vec
            * ('m, 'u_cols, 'u_cd) mat option
            * ('vt_rows, 'n, 'vt_cd) mat option
(** [gesdd ~jobz ?s ?u ?vt ?work ?iwork a] computes the singular value
    decomposition (SVD) of general rectangular matrix [a] using a divide and
    conquer method: [a = U * D * V^T] where

    - [D] is an ['m]-by-['n] matrix (the diagonal elements in [D] are singular
      values in descreasing order, and other elements are zeros),
    - [U] is an ['m]-by-['m] orthogonal matrix (the columns in [U] are left
      singular vectors), and
    - [V] is an ['n]-by-['n] orthogonal matrix (the columns in [V] are right
      singular vectors).

    @return [(s, u, vt)] with singular values [s] in descreasing order,
            left singular vectors [u] and right singular vectors [vt].
            If [u] ([vt]) is not needed, [None] is returned.
    @param jobz  the SVD job flag:
    - If [jobz] is {!Slap_common.svd_all}, all ['m] columns of [U] and all ['n]
      rows of [V^T] are returned in [u] and [vt].
      (['u_cols] = ['m] and ['vt_rows] = ['n].)
    - If [jobz] is {!Slap_common.svd_top}, the first [('m, 'n) min] columns of
      [U] and the first [('m, 'n) min] rows of [V^T] are returned in [u] and
      [vt]. (['u_cols] = [('m, 'n) min] and ['vt_rows] = [('m, 'n) min].)
    - If [jobz] is {!Slap_common.svd_overwrite}, then
    {ul
      {- if ['m >= 'n], [a] is overwritten with the first [('m, 'n) min] columns
         of [U], and all ['n] rows of [V^T] is returned in [vt], thus [vt] is
         ['n]-by-['n] and [u] is not used;}
      {- if ['m < 'n], [a] is overwritten with the first [('m, 'n) min] rows of
         [V^T], and all ['m] columns of [U] is returned in [u]; thereby [u] is
         ['m]-by-['m] and [vt] is not used.}}
      (In either case, ['u_cols] = ['m] and ['vt_rows] = ['n], but either [u] or
      [vt] should be omitted.)
    - If [jobz] is {!Slap_common.svd_no}, no singular vectors are computed.
      (['u_cols] = [z] and ['vt_rows] = [z].)
    @param s     a return location for singular values.
                 (default = an implicitly allocated vector.)
    @param u     a return location for left singular vectors.
                 (default = an implicitly allocated matrix.)
    @param vt    a return location for (transposed) right singular vectors.
                 (default = an implicitly allocated matrix.)
    @param work  default = an optimum-length vector.
    @param iwork default = an optimum-length vector.
 *)

(** {3 General eigenvalue problem (simple drivers)} *)

(** {4 geev} *)

val geev_min_lwork : ?vectors:bool -> 'n Slap_size.t -> (module Slap_size.SIZE)
(** [geev_min_lwork ?vectors n] computes the minimum length of workspace for
    [geev] routine. [n] is the number of rows or columns of a matrix.

    @param vectors whether eigenvectors are computed, or not.
                   (default = [true], i.e., they are computed.)
 *)

val geev_opt_lwork : ?vl:('n, 'n, 'vl_cd) mat option ->
                     ?vr:('n, 'n, 'vr_cd) mat option ->
                     ?wr:('n, cnt) vec ->
                     ?wi:('n, cnt) vec ->
                     ('n, 'n, 'a_cd) mat -> (module Slap_size.SIZE)
(** [geev_opt_lwork ?vl ?vr ?wr ?vi a] computes the optimum length of workspace
    for [geev] routine.

    @param vl   a return location for left eigenvectors. See LAPACK GEEV
                documentation for details about storage of complex eigenvectors.
                (default = an implicitly allocated matrix.)
      - If [vl] = [None], left eigenvectors are not computed;
      - If [vl] = [Some vl], left eigenvectors are computed.
    @param vr   a return location for right eigenvectors. See LAPACK GEEV
                documentation for details about storage of complex eigenvectors.
                (default = an implicitly allocated matrix.)
      - If [vr] = [None], right eigenvectors are not computed;
      - If [vr] = [Some vr], right eigenvectors are computed.
    @param wr   a return location for real parts of eigenvalues.
                (default = an implicitly allocated vector.)
    @param wi   a return location for imaginary parts of eigenvalues.
                (default = an implicitly allocated vector.)
 *)

val geev : ?work:('lwork, cnt) vec ->
           ?vl:('n, 'n, 'vl_cd) mat option ->
           ?vr:('n, 'n, 'vr_cd) mat option ->
           ?wr:('n, cnt) vec ->
           ?wi:('n, cnt) vec ->
           ('n, 'n, 'a_cd) mat ->
           ('n, 'n, 'vl_cd) mat option
           * ('n, cnt) vec
           * ('n, cnt) vec
           * ('n, 'n, 'vr_cd) mat option
(** [geev ?work ?vl ?vr ?wr ?wi a] computes the eigenvalues and the left and
    right eigenvectors of ['n]-by-['n] nonsymmetric matrix [a]:

    Let [w(j)] is the [j]-th eigenvalue of [a]. The [j]-th right eigenvector
    [vr(j)] satisfies [a * vr(j) = w(j) * vr(j)], and the [j]-th left
    eigenvector [vl(j)] satisfies [vl(j)^H * a = vl(j)^H * w(j)] where [vl(j)^H]
    denotes the conjugate transpose of [vl(j)]. The computed eigenvalues are
    normalized by Euclidian norm.

    @return [(vl, wr, wi, vr)] where [vl] and [vr] are left and right
            eigenvectors, and [wr] and [wi] are the real and imaginary parts
            of eigenvalues. If [vl] ([vr]) is an empty matrix, [None] is set.
    @param work default = an optimum-length vector.
    @param vl   a return location for left eigenvectors. See LAPACK GEEV
                documentation for details about storage of complex eigenvectors.
                (default = an implicitly allocated matrix.)
      - If [vl] = [None], left eigenvectors are not computed;
      - If [vl] = [Some vl], left eigenvectors are computed.
    @param vr   a return location for right eigenvectors. See LAPACK GEEV
                documentation for details about storage of complex eigenvectors.
                (default = an implicitly allocated matrix.)
      - If [vr] = [None], right eigenvectors are not computed;
      - If [vr] = [Some vr], right eigenvectors are computed.
    @param wr   a return location for real parts of eigenvalues.
                (default = an implicitly allocated vector.)
    @param wi   a return location for imaginary parts of eigenvalues.
                (default = an implicitly allocated vector.)

    @raise Failure if the function fails to converge.
 *)

(** {3 Symmetric-matrix eigenvalue and singular value problems
         (simple drivers)} *)

(** {4 syev} *)

type 'n syev_min_lwork

val syev_min_lwork : 'n Slap_size.t -> 'n syev_min_lwork Slap_size.t
(** [syev_min_lwork n] computes the minimum length of workspace for
    [syev] routine. [n] is the number of rows or columns of a matrix.
 *)

val syev_opt_lwork : ?vectors:bool -> ?up:bool -> ('n, 'n, 'cd) mat ->
                     (module Slap_size.SIZE)
(** [syev_opt_lwork ?vectors ?up a] computes the optimum length of workspace for
    [syev] routine.

    @param vectors whether eigenvectors are computed, or not.
                   (default = [false].)
      - If [vectors] = [true], eigenvectors are computed and returned in [a].
      - If [vectors] = [false], eigenvectors are not computed.
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
 *)

val syev : ?vectors:bool -> ?up:bool ->
           ?work:('lwork, cnt) vec ->
           ?w:('n, cnt) vec ->
           ('n, 'n, 'cd) mat -> ('n, 'cnt) vec
(** [syev ?vectors ?up ?work ?w a] computes the eigenvalues and the eigenvectors
    of ['n]-by-['n] symmetric matrix [a].

    @return the vector [w] of eigenvalues in ascending order.
    @param vectors whether eigenvectors are computed, or not.
                   (default = [false].)
      - If [vectors] = [true], eigenvectors are computed and returned in [a].
      - If [vectors] = [false], eigenvectors are not computed.
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param work default = an optimum-length vector.
    @param w a return location for eigenvalues.
             (default = an implicitly allocated vector.)

    @raise Failure if the function fails to converge.
 *)

(** {4 syevd} *)

val syevd_min_lwork : vectors:bool -> 'n Slap_size.t -> (module Slap_size.SIZE)
(** [syevd_min_lwork ?vectors n] computes the minimum length of workspace [work]
    for [syevd] routine. [n] is the number of rows or columns of a matrix.

    @param vectors whether eigenvectors are computed, or not.
                   (default = [false], i.e., they are not computed.)
 *)

val syevd_min_liwork : vectors:bool -> 'n Slap_size.t -> (module Slap_size.SIZE)
(** [syevd_min_liwork ?vectors n] computes the minimum length of workspace
    [iwork] for [syevd] routine. [n] is the number of rows or columns of a
    matrix.

    @param vectors whether eigenvectors are computed, or not.
                   (default = [false], i.e., they are not computed.)
 *)

val syevd_opt_lwork : ?vectors:bool -> ?up:bool ->
                      ('n, 'n, 'cd) mat -> (module Slap_size.SIZE)
(** [syevd_opt_lwork ?vectors ?up a] computes the optimum length of workspace
    [work] for [syevd] routine.

    @param vectors whether eigenvectors are computed, or not.
                   (default = [false], i.e., they are not computed.)
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
 *)

val syevd_opt_liwork : ?vectors:bool -> ?up:bool ->
                       ('n, 'n, 'cd) mat -> (module Slap_size.SIZE)
(** [syevd_opt_liwork ?vectors ?up a] computes the optimum length of workspace
    [iwork] for [syevd] routine.

    @param vectors whether eigenvectors are computed, or not.
                   (default = [false], i.e., they are not computed.)
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
 *)

val syevd : ?vectors:bool ->
            ?up:bool ->
            ?work:('lwork, cnt) vec ->
            ?iwork:('liwork, cnt) Slap_common.int32_vec ->
            ?w:('n, cnt) vec ->
            ('n, 'n, 'a_cd) mat -> ('n, 'w_cd) vec
(** [syev ?vectors ?up ?w a] computes the eigenvalues and the eigenvectors of
    ['n]-by-['n] symmetric matrix [a] using divide and conquer method.

    @return the vector [w] of eigenvalues in ascending order.
    @param vectors whether eigenvectors are computed, or not.
                   (default = [false].)
      - If [vectors] = [true], eigenvectors are computed and returned in [a].
      - If [vectors] = [false], eigenvectors are not computed.
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param work default = an optimum-length vector.
    @param iwork default = an optimum-length vector.
    @param w a return location for eigenvalues.
             (default = an implicitly allocated vector.)

    @raise Failure if the function fails to converge.
 *)

(** {4 sbev} *)

type 'n sbev_min_lwork

val sbev_min_lwork : 'n Slap_size.t -> 'n sbev_min_lwork Slap_size.t
(** [sbev_min_lwork n] computes the minimum length of workspace [work]
    for [sbev] routine. [n] is the number of rows or columns of a matrix.
 *)

val sbev : kd:'kd Slap_size.t ->
           ?z:('n, 'n, 'z_cd) mat ->
           ?up:bool ->
           ?work:('lwork, cnt) vec ->
           ?w:('n, cnt) vec ->
           (('n, 'kd) Slap_size.syband, 'n, 'a_cd) mat -> ('n, 'cnt) vec
(** [sbev ~kd ?z ?up ?work ?w ab] computes all eigenvalues and, optionally,
    eigenvectors of real symmetric band matrix [ab] store in band storage
    format.

    @return vector [w], which is overwritten.
    @param kd   the number of subdiagonals or superdiagonals.
    @param z    The eigenvectors are returned in [z] if it is given.
                They are not computed if omitted.
    @param up   default = [true]
      - If [up] = [true], then the upper triangular part of [ab] is used;
      - If [up] = [false], then the lower triangular part of [ab] is used.
    @param work workspace for [sbev]
    @param w    [w] is replaced by eigenvalues if it is given, or newly
                allocated if omitted.

    @raise Failure if the function fails to converge.
    @since 0.2.0
 *)

(** {3 Symmetric-matrix eigenvalue and singular value problems
       (expert & RRR drivers)} *)

(** {4 syevr} *)

type 'n syevr_min_lwork

val syevr_min_lwork : 'n Slap_size.t -> 'n syevr_min_lwork Slap_size.t
(** [sbevr_min_lwork n] computes the minimum length of workspace [work]
    for [syevr] routine. [n] is the number of rows or columns of a matrix.
 *)

type 'n syevr_min_liwork

val syevr_min_liwork : 'n Slap_size.t -> 'n syevr_min_liwork Slap_size.t
(** [sbevr_min_liwork n] computes the minimum length of workspace [iwork]
    for [syevr] routine. [n] is the number of rows or columns of a matrix.
 *)

val syevr_opt_lwork : ?vectors:bool ->
                      ?range:[ `A | `I of int * int | `V of float * float ] ->
                      ?up:bool ->
                      ?abstol:float ->
                      ('n, 'n, 'a_cd) mat -> (module Slap_size.SIZE)
(** [sbevr_opt_lwork ?vectors ?range ?up ?abstol a] computes the optimum length
    of workspace [work] for [syevr] routine.

    @param vectors whether eigenvectors are computed, or not.
                   (default = [false], i.e., they are not computed.)
    @param range default = [`A].
    @param up default = [true].
    @param abstol The absolute error tolerance to which each eigenvalue or
                  eigenvector is required. (default = [lamch `S].)
 *)

val syevr_opt_liwork : ?vectors:bool ->
                       ?range:[ `A | `I of int * int | `V of float * float ] ->
                       ?up:bool ->
                       ?abstol:float ->
                       ('n, 'n, 'a_cd) mat -> (module Slap_size.SIZE)
(** [sbevr_opt_liwork ?vectors ?range ?up ?abstol a] computes the optimum length
    of workspace [iwork] for [sbevr] routine.

    @param vectors whether eigenvectors are computed, or not.
                   (default = [false], i.e., they are not computed.)
    @param range default = [`A].
    @param up default = [true].
    @param abstol The absolute error tolerance to which each eigenvalue or
                  eigenvector is required. (default = [lamch `S].)
 *)

module type SYEVR_RESULT =
  sig
    type m
    type n
    val value : m Slap_size.t
                * (n, 'cnt) vec
                * (n, m, 'cnt) mat
                * ((m, m) Slap_size.add, 'cnt) Slap_common.int32_vec
  end
(** The signature of returned modules of [syevr_dyn]. *)

val syevr_dyn : ?vectors:bool ->
                ?range:[ `A | `I of int * int | `V of float * float ] ->
                ?up:bool ->
                ?abstol:float ->
                ?work:('lwork, cnt) vec ->
                ?iwork:('liwork, cnt) Slap_common.int32_vec ->
                ?w:('n, cnt) vec ->
                ?z:('n, 'k, 'z_cd) mat ->
                ?isuppz:(('k, 'k) Slap_size.add, cnt) Slap_common.int32_vec ->
                ('n, 'n, 'a_cd) mat -> (module SYEVR_RESULT with type n = 'n)
(** [syevr_dyn ?vectors ?range ?up ?abstol ?work ?iwork ?w ?z ?isuppz a]
    computes selected eigenvalues [w] and, optionally, eigenvectors [z] of a
    real symmetric matrix [a] using the Relatively Robust Representations.

    Usage:
{[
let f (type nn) ... =
  ...
  let (a : (nn, nn, _) mat) = ... in
  let module X = (val syevr_dyn ?vectors ?range ?up ?abstol
                      ?work ?iwork ?w ?z ?isuppz a
                   : SYEVR_RESULT with type n = nn) in
  let (m, w, z, isuppz) = X.value in
  ...
]}
    where type [nn] is the size of symmetric matrix [a].
    The returned module [X] contains tuple [X.value = (m, w, z, isuppz)] and
    type [X.m] for representation of the number of computed eigenvalues:

    - Size [m : X.m Slap_size.t] is the number of eigenvalues.
    - Vector [w : (X.n, _) vec] contains the [m] eigenvalues in ascending order.
    - Matrix [z : (X.n, X.m, _) mat] contains the [m] eigenvectors of dimension
      [n] in the same order.
    - [2*m]-dimensional vector
      [isuppz : ((m, m) Slap.Slap_size.add, _) Slap_common.int32_vec] indicates the
      nonzero elements in [z].

    @return the above-mentioned module [X].
    @param vectors whether eigenvectors are computed, or not.
                   (default = [false].)
      - If [vectors] = [true], eigenvectors are computed and returned in [a].
      - If [vectors] = [false], eigenvectors are not computed.
    @param range default = [`A]
      - If [range] = [`A], all eigenvalues are computed.
      - If [range] = [`I (il, iu)], eigenvalues with indices [il] to [iu] are
        computed.
      - If [range] = [`V (vl, vu)], the routine computes eigenvalues [w(i)] in
        the half-open interval: [vl < w(i) <= vu] where [vl <= vu].
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param abstol The absolute error tolerance to which each eigenvalue or
                  eigenvector is required. (default = [lamch `S].)
    @param work   default = an optimum-length vector.
    @param iwork  default = an optimum-length vector.
    @param w      a return location for eigenvalues.
                  (default = an implicitly allocated vector of the minimum
                  required dimension.)
    @param z      a return location for eigenvectors.
                  (default = an implicitly allocated matrix of the minimum
                  required dimension.)
    @param isuppz a return location for a vector to indicate the nonzero
                  elements in [z].
                  (default = an implicitly allocated matrix of the minimum
                  required dimension.)

    @raise Invalid_argument if not [X.m <= 'k]
 *)

(** {4 sygv} *)

val sygv_opt_lwork : ?vectors:bool ->
                     ?up:bool ->
                     ?itype:[ `AB | `A_B | `BA ] ->
                     ('n, 'n, 'a_cd) mat ->
                     ('n, 'n, 'b_cd) mat -> (module Slap_size.SIZE)
(** [sygv_opt_lwork ?vectors ?up ?itype a b] computes the optimum length of
    workspace [work] for [sbevr] routine.

    @param vectors whether eigenvectors are computed, or not.
                   (default = [false], i.e., they are not computed.)
    @param up      default = [true].
    @param itype   the behavior of this routine.
 *)

val sygv : ?vectors:bool ->
           ?up:bool ->
           ?work:('lwork, cnt) vec ->
           ?w:('n, cnt) vec ->
           ?itype:[ `AB | `A_B | `BA ] ->
           ('n, 'n, 'a_cd) mat ->
           ('n, 'n, 'b_cd) mat -> ('n, 'cnt) vec
(** [sygv ?vectors ?up ?work ?w ?itype a b] solves a real generalized symmetric
    definite eigenproblem:

    - [a * x = lambda * b * x] if [itype] is [`A_B];
    - [a * b * x = lambda * x] if [itype] is [`AB];
    - [b * a * x = lambda * x] if [itype] is [`BA]

    where [a] is a ['n]-by-['n] symmetric matrix, and [b] is a ['n]-by-['n]
    symmetric positive definite matrix.

    @raise Failure if the function fails to converge.
    @return vector [w] of eigenvalues in ascending order.
    @param vectors whether eigenvectors are computed, or not.
                   (default = [false].)
      - If [vectors] = [true], eigenvectors are computed and returned in [a].
      - If [vectors] = [false], eigenvectors are not computed.
    @param up default = [true].
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param work  default = an optimum-length workspace.
    @param w     a return location for eigenvalues.
                 (default = an implicitly allocated vector.)
    @param itype the behavior of this routine.

    @raise Failure if the function fails to converge.
 *)

(** {4 sbgv} *)

val sbgv : ka:'ka Slap_size.t -> kb:'kb Slap_size.t ->
           ?z:('n, 'n, 'z_cd) mat ->
           ?up:bool ->
           ?work:('lwork, cnt) vec ->
           ?w:('n, cnt) vec ->
           (('n, 'ka) Slap_size.syband, 'n, 'ab_cd) mat ->
           (('n, 'kb) Slap_size.syband, 'n, 'bb_cd) mat -> ('n, 'cnt) vec
(** [sbgv ~ka ~kb ?z ?up ?work ?w ab bb] solves a general eigenvalue problem
    [ab * z = (lambda) * bb * z] where [ab] is a ['n]-by-['n] symmetric band
    matrix with [ka] subdiagonals, and [bb] is a ['n]-by-['n] symmetric
    positive-definite band matrix with [kb] subdiagonals. Both [ab] and [bb]
    are stored in band storage format.

    @return vector [w], which is overwritten.
    @param ka   the number of subdiagonals or superdiagonals of [ab].
    @param kb   the number of subdiagonals or superdiagonals of [bb].
    @param z    The eigenvectors are returned in [z] if it is given.
                They are not computed if omitted.
    @param up   default = [true]
      - If [up] = [true], then the upper triangular part of [ab] is used;
      - If [up] = [false], then the lower triangular part of [ab] is used.
    @param work workspace for [sbgv]
    @param w    [w] is replaced by eigenvalues if it is given, or newly
                allocated if omitted.

    @raise Failure if the function fails to converge.
    @since 0.2.0
 *)
