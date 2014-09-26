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

val dot : x:('n, 'x_cd) vec -> ('n, 'y_cd) vec -> float
(** [dot ~x y]
    @return the inner product of the vector [x] and [y].
 *)

val asum : ('n, 'x_cd) vec -> float
(** [asum x]
    @return the sum of absolute values of elements in the vector [x].
 *)

(** {3 Level 2} *)

val sbmv : k:'k Size.t ->
           ?y:('n, 'y_cd) vec ->
           (('n, 'k) Size.syband, 'n, 'a_cd) mat ->
           ?up:bool ->
           ?alpha:float ->
           ?beta:float ->
           ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [sbmv ~k ?y a ?up ?alpha ?beta x] computes [y := alpha * a * x + beta * y]
    where [a] is a ['n]-by-['n] symmetric band matrix with [k]
    super-(or sub-)diagonals, and [x] and [y] are ['n]-dimensional vectors.
    @return vector [y], which is overwritten.
    @param k the number of superdiangonals or subdiangonals
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.
    @param alpha default = [1.0]
    @param beta default = [0.0]
    @since 0.2.0
 *)

val ger : ?alpha:float -> ('m, 'x_cd) vec -> ('n, 'y_cd) vec ->
          ('m, 'n, 'a_cd) mat -> ('m, 'n, 'a_cd) mat
(** [ger ?alpha x y a] computes [a := alpha * x * y^T + a] with
    the general matrix [a], the vector [x] and
    the transposed vector [y^T] of [y].
    @return [a], which is overwritten.
    @param alpha default = [1.0]
 *)

val syr : ?alpha:float -> ?up:bool -> ('n, 'x_cd) vec ->
          ('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat
(** [syr ?alpha x a] computes [a := alpha * x * x^T + a] with
    the symmetric matrix [a], the vector [x] and
    the transposed vector [x^T] of [x].
    @return [a], which is overwritten.
    @param alpha default = [1.0]
    @param up    default = [true], i.e., the upper triangular part of [a] is
                 supplied.
 *)

(** {2 LAPACK interface} *)

(** {3 Auxiliary routines} *)

(** {4 lansy} *)

type ('m, 'a) lansy_min_lwork

val lansy_min_lwork : 'n Size.t -> 'a Common.norm4 ->
                      ('n, 'a) lansy_min_lwork Size.t

val lansy : ?up:bool ->
            ?norm:'norm Common.norm4 ->
            ?work:('lwork, cnt) vec -> ('n, 'n, 'cd) mat -> float
(** [lansy ?up ?norm ?work a]
    @return the norm of matrix [a].
    @param up   default = [true]
    @param norm default = [Slap.Common.norm_1]
    @param work workspace (the size must be equal to or greater than
                [lansy_min_lwork n norm].)
 *)

(** {4 lamch} *)

val lamch : [ `B | `E | `L | `M | `N | `O | `P | `R | `S | `U ] -> float
(** [lamch cmach] see LAPACK documentation. *)

(** {3 Linear equations (computational routines)} *)

(** {4 orgqr} *)

type 'n orgqr_min_lwork

val orgqr_min_lwork : n:'n Size.t -> 'n orgqr_min_lwork Size.t

val orgqr_opt_lwork : tau:('k, cnt) vec ->
                      ('m, 'n, 'cd) mat -> (module Size.SIZE)

val orgqr_dyn : ?work:('lwork, cnt) vec ->
                tau:('k, cnt) vec ->
                ('m, 'n, 'cd) mat -> unit
(** [orgqr_dyn ?work ~tau a] generates the orthogonal matrix [Q] of the QR
    factorization formed by [geqrf]/[geqpf].

    Matrix [a] and vector [tau] must satisfy the following inequality:
    [(Mat.dim1 a) >= (Mat.dim2 a) >= (Vec.dim tau)], i.e., ['m >= 'n >= 'k].
 *)

(** {4 ormqr} *)

type ('r, 'm, 'n) ormqr_min_lwork

val ormqr_min_lwork : side:('r, 'm, 'n) Common.side ->
                      m:'m Size.t ->
                      n:'n Size.t ->
                      ('r, 'm, 'n) ormqr_min_lwork Size.t

val ormqr_opt_lwork : side:('r, 'm, 'n) Common.side ->
                      trans:(('r, 'r, _) mat -> ('r, 'r, _) mat) Common.trans2 ->
                      tau:('k, cnt) vec ->
                      ('r, 'k, 'a_cd) mat ->
                      ('m, 'n, 'c_cd) mat -> (module Size.SIZE)

val ormqr_dyn : side:('r, 'm, 'n) Common.side ->
                trans:(('r, 'r, _) mat -> ('r, 'r, _) mat) Common.trans2 ->
                ?work:('lwork, cnt) vec ->
                tau:('k, cnt) vec ->
                ('r, 'k, 'a_cd) mat ->
                ('m, 'n, 'c_cd) mat -> unit
(** [ormqr_dyn ~side ~trans ?work ~tau a c] multiplies a matrix [c] by the
    orthogonal matrix [Q] of the QR factorization formed by [geqrf]/[geqpf].

    Type parameters ['k], ['m] and ['n] must satisfy the following inequality:
    - ['m >= 'k] if [side] = [Common.left];
    - ['n >= 'k] if [side] = [Common.right].
 *)

(** {4 gecon} *)

type 'n gecon_min_lwork

val gecon_min_lwork : 'n Size.t -> 'n gecon_min_lwork Size.t

type 'n gecon_min_liwork

val gecon_min_liwork : 'n Size.t -> 'n gecon_min_liwork Size.t

val gecon : ?norm:_ Common.norm2 ->
            ?anorm:float ->
            ?work:('lwork, cnt) vec ->
            ?iwork:('liwork, cnt) Common.int32_vec ->
            ('n, 'n, 'cd) mat -> float
(** [gecon ?norm ?anorm ?work ?iwork a] estimates the reciprocal of the
    condition number of general matrix [a]. *)

(** {4 sycon} *)

type 'n sycon_min_lwork

val sycon_min_lwork : 'n Size.t -> 'n sycon_min_lwork Size.t

type 'n sycon_min_liwork

val sycon_min_liwork : 'n Size.t -> 'n sycon_min_liwork Size.t

val sycon : ?up:bool ->
            ?ipiv:('n, cnt) Common.int32_vec ->
            ?anorm:float ->
            ?work:('lwork, cnt) vec ->
            ?iwork:('liwork, cnt) Common.int32_vec ->
            ('n, 'n, 'cd) mat -> float

(** {4 pocon} *)

type 'n pocon_min_lwork

val pocon_min_lwork : 'n Size.t -> 'n pocon_min_lwork Size.t

type 'n pocon_min_liwork

val pocon_min_liwork : 'n Size.t -> 'n pocon_min_liwork Size.t

val pocon : ?up:bool ->
            ?anorm:float ->
            ?work:('lwork, cnt) vec ->
            ?iwork:('liwork, cnt) Common.int32_vec ->
            ('n, 'n, 'cd) mat -> float

(** {3 Least squares (expert drivers)} *)

(** {4 gelsy} *)

type ('m, 'n, 'nrhs) gelsy_min_lwork

val gelsy_min_lwork : m:'m Size.t ->
                      n:'n Size.t ->
                      nrhs:'nrhs Size.t ->
                      ('m, 'n, 'nrhs) gelsy_min_lwork Size.t

val gelsy_opt_lwork : ('m, 'n, 'a_cd) mat ->
                      ('n, 'nrhs, 'b_cd) mat -> (module Size.SIZE)

val gelsy : ('m, 'n, 'a_cd) mat ->
            ?rcond:float ->
            ?jpvt:('n, cnt) Common.int32_vec ->
            ?work:('lwork, cnt) vec ->
            ('n, 'nrhs, 'b_cd) mat -> int

(** {4 gelsd} *)

type ('m, 'n, 'nrhs) gelsd_min_lwork

val gelsd_min_lwork : m:'m Size.t ->
                      n:'n Size.t ->
                      nrhs:'nrhs Size.t ->
                      ('m, 'n, 'nrhs) gelsd_min_lwork Size.t

val gelsd_opt_lwork : ('m, 'n, 'a_cd) mat ->
                      ('n, 'nrhs, 'b_cd) mat -> (module Size.SIZE)

type ('m, 'n, 'nrhs) gelsd_min_iwork

val gelsd_min_iwork : 'm Size.t ->
                      'n Size.t ->
                      ('m, 'n, 'nrhs) gelsd_min_iwork Size.t

val gelsd : ('m, 'n, 'a_cd) mat ->
            ?rcond:float ->
            ?s:(('m, 'n) Size.min, cnt) vec ->
            ?work:('lwork, cnt) vec ->
            ?iwork:('liwork, cnt) vec ->
            ('n, 'nrhs, 'b_cd) mat -> int

(** {4 gelss} *)

type ('m, 'n, 'nrhs) gelss_min_lwork

val gelss_min_lwork : m:'m Size.t ->
                      n:'n Size.t ->
                      nrhs:'nrhs Size.t ->
                      ('m, 'n, 'nrhs) gelss_min_lwork Size.t

val gelss_opt_lwork : ('m, 'n, 'a_cd) mat ->
                      ('n, 'nrhs, 'b_cd) mat -> (module Size.SIZE)

val gelss : ('m, 'n, 'a_cd) mat ->
            ?rcond:float ->
            ?s:(('m, 'n) Size.min, cnt) vec ->
            ?work:('lwork, cnt) vec ->
            ('n, 'nrhs, 'b_cd) mat -> int

(** {3 General SVD routines} *)

(** {4 gesvd} *)

type ('m, 'n) gesvd_min_lwork

val gesvd_min_lwork : m:'m Size.t -> n:'n Size.t ->
                      ('m, 'n) gesvd_min_lwork Size.t

val gesvd_opt_lwork : jobu:('u_cols, 'm, ('m, 'n) Size.min,
                            Size.z, Size.z) Common.svd_job ->
                      jobvt:('vt_rows, 'n, ('m, 'n) Size.min,
                             Size.z, Size.z) Common.svd_job ->
                      ?s:(('m, 'n) Size.min, cnt) vec ->
                      ?u:('m, 'u_cols, 'u_cd) mat ->
                      ?vt:('vt_rows, 'n, 'vt_cd) mat ->
                      ('m, 'n, 'a_cd) mat -> (module Size.SIZE)

val gesvd : jobu:('u_cols, 'm, ('m, 'n) Size.min,
                  Size.z, Size.z) Common.svd_job ->
            jobvt:('vt_rows, 'n, ('m, 'n) Size.min,
                   Size.z, Size.z) Common.svd_job ->
            ?s:(('m, 'n) Size.min, cnt) vec ->
            ?u:('m, 'u_cols, 'u_cd) mat ->
            ?vt:('vt_rows, 'n, 'vt_cd) mat ->
            ?work:('lwork, cnt) vec ->
            ('m, 'n, 'a_cd) mat ->
            (('m, 'n) Size.min, 'cnt) vec *
              ('m, 'u_cols, 'cnt) mat *
                ('vt_rows, 'n, 'cnt) mat

(** {4 gesdd} *)

type ('m, 'n) gesdd_liwork

val gesdd_liwork : m:'m Size.t -> n:'n Size.t -> ('m, 'n) gesdd_liwork Size.t

type ('m, 'n, 'jobz) gesdd_min_lwork

val gesdd_min_lwork : jobz:('u_cols * 'vt_rows,
                            'm * 'n,
                            ('m, 'n) Size.min * ('m, 'n) Size.min,
                            Size.z * 'n,
                            'm * Size.z) Common.svd_job ->
                      m:'m Size.t -> n:'n Size.t -> unit ->
                      ('m, 'n, 'u_cols * 'vt_rows) gesdd_min_lwork Size.t

val gesdd_opt_lwork : jobz:('u_cols * 'vt_rows,
                            'm * 'n,
                            ('m, 'n) Size.min * ('m, 'n) Size.min,
                            Size.z * 'n,
                            'm * Size.z) Common.svd_job ->
                      ?s:(('m, 'n) Size.min, cnt) vec ->
                      ?u:('m, 'u_cols, 'u_cd) mat ->
                      ?vt:('vt_rows, 'n, 'vt_cd) mat ->
                      ?iwork:('liwork, cnt) Common.int32_vec ->
                      ('m, 'n, 'a_cd) mat -> (module Size.SIZE)

val gesdd : jobz:('u_cols * 'vt_rows,
                  'm * 'n,
                  ('m, 'n) Size.min * ('m, 'n) Size.min,
                  'm * 'n,
                  Size.z * Size.z) Common.svd_job ->
            ?s:(('m, 'n) Size.min, cnt) vec ->
            ?u:('m, 'u_cols, 'u_cd) mat ->
            ?vt:('vt_rows, 'n, 'vt_cd) mat ->
            ?work:('lwork, cnt) vec ->
            ?iwork:('liwork, cnt) Common.int32_vec ->
            ('m, 'n, 'a_cd) mat ->
            (('m, 'n) Size.min, 'cnt) vec *
              ('m, 'u_cols, 'u_cd) mat * ('vt_rows, 'n, 'vt_cd) mat
(** [gesdd ~jobz ?s ?u ?vt ?work ?iwork a] computes the singular value
    decomposition of a general rectangular matrix using a divide and conquer
    method.

    - If [jobz] is {!Slap.Common.svd_all}, the all left and right singular
      vectors are stored into ['m]-by-['m] matrix [u] and ['n]-by-['n] matrix
      [vt], respectively.
    - If [jobz] is {!Slap.Common.svd_top}, the top [(min m n)] left and right
      singular vectors are computed. In this case, the sizes of matrices [u] and
      [vt] are ['m]-by-[('m,'n) Size.min] and [('m,'n) Size.min]-by-['n],
      respectively.
    - If [jobz] is {!Slap.Common.svd_overwrite}, then
    {ul
      {- if ['m >= 'n], [a] is overwritten with the first [(min m n)] columns of
         [u], and all ['n] rows of [vt] is computed, thus [vt] has ['n]-by-['n]
         size and [u] is not used;}
      {- if ['m < 'n], [a] is overwritten with the first [(min m n)] rows of
         [vt], and all ['m] columns of [u] is computed; thereby [u] has
         ['m]-by-['m] size and [vt] is not used.}}
    - If [jobz] is {!Slap.Common.svd_no}, no singular vectors are computed.

    @return [(s, u, vt)] with vector [s] containing singular values, unitary
            matrices [u] and [vt].
 *)

(** {3 General eigenvalue problem (simple drivers)} *)

(** {4 geev} *)

val geev_min_lwork : ?vectors:bool -> 'n Size.t -> (module Size.SIZE)

val geev_opt_lwork : ?vl:('n, 'n, 'vl_cd) mat option ->
                     ?vr:('n, 'n, 'vr_cd) mat option ->
                     ?wr:('n, cnt) vec ->
                     ?wi:('n, cnt) vec ->
                     ('n, 'n, 'a_cd) mat -> (module Size.SIZE)

val geev : ?work:('lwork, cnt) vec ->
           ?vl:('n, 'n, 'vl_cd) mat option ->
           ?vr:('n, 'n, 'vr_cd) mat option ->
           ?wr:('n, cnt) vec ->
           ?wi:('n, cnt) vec ->
           ('n, 'n, 'a_cd) mat ->
           ('n, 'n, 'vl_cd) mat option *
             ('n, cnt) vec *
               ('n, cnt) vec *
                 ('n, 'n, 'vr_cd) mat option

(** {3 Symmetric-matrix eigenvalue and singular value problems
         (simple drivers)} *)

(** {4 syev} *)

type 'n syev_min_lwork

val syev_min_lwork : 'n Size.t -> 'n syev_min_lwork Size.t

val syev_opt_lwork : ?vectors:bool -> ?up:bool -> ('n, 'n, 'cd) mat ->
                     (module Size.SIZE)

val syev : ?vectors:bool -> ?up:bool -> ?w:('n, cnt) vec ->
           ('n, 'n, 'cd) mat -> ('n, 'cnt) vec

(** {4 syevd} *)

val syevd_min_lwork : vectors:bool -> 'n Size.t -> (module Size.SIZE)

val syevd_min_liwork : vectors:bool -> 'n Size.t -> (module Size.SIZE)

val syevd_opt_lwork : ?vectors:bool -> ?up:bool ->
                      ('n, 'n, 'cd) mat -> (module Size.SIZE)

val syevd_opt_liwork : ?vectors:bool -> ?up:bool ->
                       ('n, 'n, 'cd) mat -> (module Size.SIZE)

val syevd : ?vectors:bool ->
            ?up:bool ->
            ?work:('lwork, cnt) vec ->
            ?iwork:('liwork, cnt) Common.int32_vec ->
            ?w:('n, cnt) vec ->
            ('n, 'n, 'a_cd) mat -> ('n, 'w_cd) vec

(** {4 sbev} *)

type 'n sbev_min_lwork

val sbev_min_lwork : 'n Size.t -> 'n sbev_min_lwork Size.t

val sbev : kd:'kd Size.t ->
           ?z:('n, 'n, 'z_cd) mat ->
           ?up:bool ->
           ?work:('lwork, cnt) vec ->
           ?w:('n, cnt) vec ->
           (('n, 'kd) Size.syband, 'n, 'a_cd) mat -> ('n, 'cnt) vec
(** [sbev ~kd ?z ?up ?work ?w ab] computes all eigenvalues and, optionally,
    eigenvectors of real symmetric band matrix [ab] store in band storage
    format.

    @return vector [w], which is overwritten.
    @param kd   the number of subdiagonals or superdiagonals.
    @param z    eigenvectors are returned in [z] if it is given.
    @param up   default = [true]
      - If [up] = [true], then the upper triangular part of [ab] is used;
      - If [up] = [false], then the lower triangular part of [ab] is used.
    @param work workspace for [sbev]
    @param w    [w] is replaced by eigenvalues if it is given, or newly
                allocated if omitted.

    @raise if the function fails to converge.
    @since 0.2.0
 *)

(** {3 Symmetric-matrix eigenvalue and singular value problems
       (expert & RRR drivers)} *)

(** {4 syevr} *)

type 'n syevr_min_lwork

val syevr_min_lwork : 'n Size.t -> 'n syevr_min_lwork Size.t

type 'n syevr_min_liwork

val syevr_min_liwork : 'n Size.t -> 'n syevr_min_liwork Size.t

val syevr_opt_lwork : ?vectors:bool ->
                      ?range:[ `A | `I of int * int | `V of float * float ] ->
                      ?up:bool ->
                      ?abstol:float ->
                      ('n, 'n, 'a_cd) mat -> (module Size.SIZE)

val syevr_opt_liwork : ?vectors:bool ->
                       ?range:[ `A | `I of int * int | `V of float * float ] ->
                       ?up:bool ->
                       ?abstol:float ->
                       ('n, 'n, 'a_cd) mat -> (module Size.SIZE)

module type SYEVR_RESULT =
  sig
    type m
    type n
    val value : m Size.t *
                  (n, 'cnt) vec *
                    (n, m, 'cnt) mat *
                      ((m, m) Size.add, 'cnt) Common.int32_vec
  end
(** The signature of returned modules of [syevr_dyn]. *)

type 'n syevr_result = (module SYEVR_RESULT with type n = 'n)

val syevr_dyn : ?vectors:bool ->
                ?range:[ `A | `I of int * int | `V of float * float ] ->
                ?up:bool ->
                ?abstol:float ->
                ?work:('lwork, cnt) vec ->
                ?iwork:('liwork, cnt) Common.int32_vec ->
                ?w:('n, cnt) vec ->
                ?z:('n, 'k, 'z_cd) mat ->
                ?isuppz:(('k, 'k) Size.add, cnt) Common.int32_vec ->
                ('n, 'n, 'a_cd) mat -> 'n syevr_result
(** [syevr_dyn ?vectors ?range ?up ?abstol ?work ?iwork ?w ?z ?isuppz a]
    computes selected eigenvalues [w] and, optionally, eigenvectors [z] of a
    real symmetric matrix [a].

    Usage:
{[
module X = (val syevr_dyn ?vectors ?range ?up ?abstol ?work ?iwork ?w ?z ?isuppz a
                : SYEVR_RESULT with type N.n = nn)
]}
    where type [nn] is the size of symmetric matrix [a].
    The returned module [X] contains tuple [X.value = (m, w, z, isuppz)] and
    type [X.m] for the size [m]:

    - size [m] (with type [X.m]) is the number of eigenvalues.
    - [n]-dimensional vector [w] contains the [m] eigenvalues in ascending order.
    - [n]-by-[m] matrix [z] contains the [m] eigenvectors in same order.
    - [2*m]-dimensional vector [isuppz] indicates the nonzero elements in [z].
    *)

(** {4 sygv} *)

val sygv_opt_lwork : ?vectors:bool ->
                     ?up:bool ->
                     ?itype:[ `AB | `A_B | `BA ] ->
                     ('n, 'n, 'a_cd) mat ->
                     ('n, 'n, 'b_cd) mat -> (module Size.SIZE)

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
    - [b * a * x = lambda * x] if [itype] is [`BA].

    @raise Failure if the function fails to converge.
    @return vector [w] of eigenvalues in ascending order.
    @param vectors default = [true].
    @param up default = [true].
    @param work default = an optimum-length workspace.
    @param w default = a newly-allocated ['n]-dimensional vector.
    @param a a ['n]-by-['n] symmetric matrix.
    @param b a ['n]-by-['n] summetric positive definite matrix.
    *)
