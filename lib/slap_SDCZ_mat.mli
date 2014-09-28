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

module type CNTMAT =
  sig
    type m (** A generative phantom type. *)
    type n (** A generative phantom type. *)

    val value : (m, n, 'cnt) mat
    (** A dynamically-sized contiguous matrix with type like
        [exists m, n. (m, n, 'cnt) mat]. *)
  end
(** The signature of modules containing dynamically-sized contiguous matrices.
 *)

module type DSCMAT =
  sig
    type m (** A generative phantom type. *)
    type n (** A generative phantom type. *)

    val value : (m, n, dsc) mat
    (** A dynamically-sized discrete matrix with type like
        [exists m, n. (m, n, dsc) vec]. *)
  end
(** The signature of modules containing dynamically-sized discrete matrices. *)

val cnt : ('m, 'n, cnt) mat -> ('m, 'n, 'cnt) mat
(** Recover polymorphism of the fifth type parameter. *)

(** {2 Creation of matrices} *)

val empty : (Size.z, Size.z, 'cnt) mat
(** An empty matrix. *)

val create : 'm Size.t -> 'n Size.t -> ('m, 'n, 'cnt) mat
(** [create m n]
    @return a fresh [m]-by-[n] matrix (not initialized).
 *)

val make : 'm Size.t -> 'n Size.t -> num_type -> ('m, 'n, 'cnt) mat
(** [make m n x]
    @return a fresh [m]-by-[n] matrix initialized with [x].
 *)

val make0 : 'm Size.t -> 'n Size.t -> ('m, 'n, 'cnt) mat
(** [make0 m n]
    @return a fresh [m]-by-[n] matrix initialized with [0].
 *)

val make1 : 'm Size.t -> 'n Size.t -> ('m, 'n, 'cnt) mat
(** [make1 m n]
    @return a fresh [m]-by-[n] matrix initialized with [1].
 *)

val identity : 'n Size.t -> ('n, 'n, 'cnt) mat
(** [identity n]
    @return a fresh [n]-by-[n] identity matrix.
 *)

val init : 'm Size.t -> 'n Size.t ->
                (int -> int -> num_type) -> ('m, 'n, 'cnt) mat
(** An alias of [init_cols]. *)

val init_cols : 'm Size.t -> 'n Size.t ->
                (int -> int -> num_type) -> ('m, 'n, 'cnt) mat
(** [init_cols m n f] returns a fresh [m]-by-[n] matrix whose
    the [(i,j)] element is initialized by the result of calling [f i j].
    The elements are passed column-wise.
 *)

val init_rows : 'm Size.t -> 'n Size.t ->
                (int -> int -> num_type) -> ('m, 'n, 'cnt) mat
(** [init_rows m n f] returns a fresh [m]-by-[n] matrix whose
    the [(i,j)] element is initialized by the result of calling [f i j].
    The elements are passed row-wise.
 *)

(** {2 Accessors} *)

val dim : ('m, 'n, 'cd) mat -> 'm Size.t * 'n Size.t
(** [dim a] is [(dim1 a, dim2 a)]. *)

val dim1 : ('m, 'n, 'cd) mat -> 'm Size.t
(** [dim1 a]
    @return the number of rows in [a].
 *)

val dim2 : ('m, 'n, 'cd) mat -> 'n Size.t
(** [dim1 a]
    @return the number of columns in [a].
 *)

val get_dyn : ('m, 'n, 'cd) mat -> int -> int -> num_type
(** [get_dyn a i j]
    @return the [(i,j)] element of the matrix [a].
 *)

val set_dyn : ('m, 'n, 'cd) mat -> int -> int -> num_type -> unit
(** [set_dyn a i j x] assigns [x] to the [(i,j)] element of the matrix [a].
 *)

val unsafe_get : ('m, 'n, 'cd) mat -> int -> int -> num_type
(** Like {!Slap.Mat.get_dyn}, but size checking is not always performed. *)

val unsafe_set : ('m, 'n, 'cd) mat -> int -> int -> num_type -> unit
(** Like {!Slap.Mat.set_dyn}, but size checking is not always performed. *)

val col_dyn : ('m, 'n, 'cd) mat -> int -> ('m, 'cnt) vec
(** [col_dyn a i]
    @return the [i]-th column of the matrix [a]. The data are shared.
 *)

val row_dyn : ('m, 'n, 'cd) mat -> int -> ('n, dsc) vec
(** [row_dyn a i]
    @return the [i]-th row of the matrix [a]. The data are shared.
 *)

val copy_row_dyn : ('m, 'n, 'cd) mat -> int -> ('n, 'cnt) vec
(** [copy_row_dyn a i] is [Vec.copy (Mat.row_dyn a i)]. *)

val diag : ('n, 'n, 'cd) mat -> ('n, dsc) vec
(** [diag a]
    @return the diagonal elements of the matrix [a]. The data are shared.
 *)

val copy_diag : ('n, 'n, 'cd) mat -> ('n, 'cnt) vec
(** [copy_diag a] is [Vec.copy (Mat.diag a)]. *)

val as_vec : ('m, 'n, cnt) mat -> (('m, 'n) Size.mul, 'cnt) vec
(** [as_vec a]
    @return the vector containing all elements of the matrix in column-major
    order. The data are shared.
 *)

(** {2 Basic operations} *)

val fill : ('m, 'n, 'cd) mat -> num_type -> unit
(** Fill the given matrix with the given value.
    @since 0.1.0
 *)

val copy : ?uplo:[ `L | `U ] ->
           ?b:('m, 'n, 'b_cd) mat ->
           ('m, 'n, 'a_cd) mat -> ('m, 'n, 'b_cd) mat
(** [copy ?uplo ?b a] copies the matrix [a] into the matrix [b] with
    the LAPACK function [lacpy].
    - If [uplo] is omitted, all elements in [a] is copied.
    - If [uplo] is [`U], the upper trapezoidal part of [a] is copied.
    - If [uplo] is [`L], the lower trapezoidal part of [a] is copied.
    @return [b], which is overwritten.
    @param uplo default = all elements in [a] is copied.
    @param b    default = a fresh matrix.
 *)

val of_col_vecs_dyn : 'm Size.t ->
                      'n Size.t ->
                      ('m, cnt) vec array ->
                      ('m, 'n, 'cnt) mat

(** {2 Type conversion} *)

val to_array : ('m, 'n, 'cd) mat -> num_type array array
(** [to_array a]
    @return the array of arrays of all the elements of [a].
 *)

val of_array_dyn : 'm Size.t -> 'n Size.t ->
                   num_type array array ->
                   ('m, 'n, 'cnt) mat
(** Build a matrix initialized from the given array of arrays.
    @raise Invalid_argument the given array of arrays is not rectangular or
    its size is not [m]-by-[n].
 *)

val of_array : num_type array array -> (module CNTMAT)
(** [module M = (val of_array aa : CNTMAT)]
    @return module [M] containing the matrix [M.value] that
    has the type [(M.m, M.n, 'cnt) mat] with a generative phantom types [M.m]
    and [M.n] as a package of an existential quantified sized type like
    [exists m, n. (m, n, 'cnt) mat].
    @raise Invalid_argument the given array of arrays is not rectangular.
 *)

module Of_array (X : sig val value : num_type array array end) : CNTMAT
(** A functor vesion of [of_array]. *)

val to_list : ('m, 'n, 'cd) mat -> num_type list list
(** [to_list a]
    @return the list of lists of all the elements of [a].
 *)

val of_list_dyn : 'm Size.t -> 'n Size.t ->
                  num_type list list ->
                  ('m, 'n, 'cnt) mat
(** Build a matrix initialized from the given list of lists.
    @raise Invalid_argument the given list of lists is not rectangular or
    its size is not [m]-by-[n].
 *)

val of_list : num_type list list -> (module CNTMAT)
(** [module M = (val of_list ll : CNTMAT)]
    @return module [M] containing the matrix [M.value] that
    has the type [(M.m, M.n, 'cnt) mat] with a generative phantom types [M.m]
    and [M.n] as a package of an existential quantified sized type like
    [exists m, n. (m, n, 'cnt) mat].
    @raise Invalid_argument the given list of lists is not rectangular.
 *)

module Of_list (X : sig val value : num_type list list end) : CNTMAT
(** A functor vesion of [of_list]. *)

(** {2 Iterators} *)

val map : (num_type -> num_type) -> ?b:('m, 'n, 'b_cd) mat ->
          ('m, 'n, 'a_cd) mat -> ('m, 'n, 'b_cd) mat

val mapi : (int -> int -> num_type -> num_type) -> ?b:('m, 'n, 'b_cd) mat ->
           ('m, 'n, 'a_cd) mat -> ('m, 'n, 'b_cd) mat

val fold_left : ('accum -> ('m, 'x_cd) vec -> 'accum) ->
                'accum ->
                ('m, 'n, 'a_cd) mat ->
                'accum
(** [fold_left f init a] folds column vectors of matrix [a] by [f] in the order
    left to right.
 *)

val fold_lefti : (int -> 'accum -> ('m, 'x_cd) vec -> 'accum) ->
                 'accum ->
                 ('m, 'n, 'a_cd) mat ->
                 'accum
(** [fold_lefti f init a] folds column vectors of matrix [a] by [f] in the order
    left to right.
 *)

val fold_right : (('m, 'x_cd) vec -> 'accum -> 'accum) ->
                 ('m, 'n, 'a_cd) mat ->
                 'accum ->
                 'accum
(** [fold_right f a init] folds column vectors of matrix [a] by [f] in the
    order right to left.
 *)

val fold_righti : (int -> ('m, 'x_cd) vec -> 'accum -> 'accum) ->
                  ('m, 'n, 'a_cd) mat ->
                  'accum ->
                  'accum
(** [fold_righti f a init] folds column vectors of matrix [a] by [f] in the
    order right to left.
 *)

val fold_top : ('accum -> ('n, dsc) vec -> 'accum) ->
               'accum ->
               ('m, 'n, 'a_cd) mat ->
               'accum
(** [fold_top f init a] folds row vectors of matrix [a] by [f] in the order
    top to bottom.
 *)

val fold_topi : (int -> 'accum -> ('n, dsc) vec -> 'accum) ->
                'accum ->
                ('m, 'n, 'a_cd) mat ->
                'accum
(** [fold_topi f init a] folds row vectors of matrix [a] by [f] in the order
    top to bottom.
 *)

val fold_bottom : (('m, dsc) vec -> 'accum -> 'accum) ->
                  ('m, 'n, 'a_cd) mat ->
                  'accum ->
                  'accum
(** [fold_bottom f a init] folds row vectors of matrix [a] by [f] in the
    order bottom to top.
 *)

val fold_bottomi : (int -> ('m, dsc) vec -> 'accum -> 'accum) ->
                   ('m, 'n, 'a_cd) mat ->
                   'accum ->
                   'accum
(** [fold_bottomi f a init] folds row vectors of matrix [a] by [f] in the
    order bottom to top.
 *)

val replace_all : ('m, 'n, 'cd) mat -> (num_type -> num_type) -> unit
(** [replace_all a f] modifies the matrix [a] in place
    -- the [(i,j)]-element [aij] of [a] will be set to [f aij].
 *)

val replace_alli : ('m, 'n, 'cd) mat ->
                   (int -> int -> num_type -> num_type) -> unit
(** [replace_all a f] modifies the matrix [a] in place
    -- the [(i,j)]-element [aij] of [a] will be set to [f i j aij].
 *)

(** {2 Matrix transformations} *)

val transpose_copy : ('m, 'n, 'a_cd) mat -> ('n, 'm, 'b_cd) mat -> unit
(** [transpose_copy a b] copies the transpose of [a] into [b].
    @since 0.1.0
 *)

val transpose : ('m, 'n, 'cd) mat -> ('n, 'm, 'cnt) mat
(** [transpose a] returns the transpose of [a].
    @since 0.1.0
 *)

val detri : ?up:bool -> ('n, 'n, 'cd) mat -> unit
(** [detri ?up a] converts triangular matrix [a] to a symmetric matrix, i.e.,
    copies the upper or lower triangular part of [a] into another part.
    @param up default = [true]
    @since 0.1.0
 *)

val packed : ?up:bool ->
             ?x:('n Size.packed, cnt) vec ->
             ('n, 'n, 'cd) mat ->
             ('n Size.packed, 'cnt) vec
(** [packed ?up ?x a] transforms matrix [a] into packed storage format.
    @return vector [x], which is overwritten.
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is packed;
      - If [up] = [false], then the lower triangular part of [a] is packed.
    @since 0.2.0
 *)

val unpacked : ?up:bool ->
               ?fill_num:num_type option ->
               ?a:('n, 'n, 'cd) mat ->
               ('n Size.packed, cnt) vec ->
               ('n, 'n, 'cd) mat
(** [unpacked ?up ?fill_num ?a x] generates an upper or lower triangular matrix
    from packed-storage-format vector [x].
    @return matrix [a], which is overwritten.
    @param up default = [true]
      - If [up] = [true], then the upper triangular matrix is generated;
      - If [up] = [false], then the lower triangular matrix is generated.
    @param fill_num default = [Some 0]
      - If [fill_num] is [None], the elements in the generated matrix are not
        initialized;
      - If [fill_num] is [Some c], the elements in the generated matrix are
        initialized by [c].
    @since 0.2.0
 *)

val geband_dyn : 'kl Size.t -> 'ku Size.t ->
                 ?b:(('m, 'n, 'kl, 'ku) Size.geband, 'n, 'b_cd) mat ->
                 ('m, 'n, 'a_cd) mat ->
                 (('m, 'n, 'kl, 'ku) Size.geband, 'n, 'b_cd) mat
(** [geband_dyn kl ku ?b a] converts matrix [a] into a matrix stored in band
    storage.
    @return matrix [b], which is overwritten.
    @param kl the number of subdiagonals
    @param ku the number of superdiagonals

    @raise Invalid_arg if [kl >= dim1 a] or [ku >= dim2 a].
    @since 0.2.0
 *)

val ungeband : 'm Size.t -> 'kl Size.t -> 'ku Size.t ->
               ?fill_num:num_type option ->
               ?a:('m, 'n, 'a_cd) mat ->
               (('m, 'n, 'kl, 'ku) Size.geband, 'n, 'b_cd) mat ->
               ('m, 'n, 'a_cd) mat
(** [ungeband m kl ku ?a b] converts matrix [b] stored in band storage into
    a matrix stored in the normal order.
    @return matrix [a], which is overwritten.
    @param m the number of rows in [a]
    @param kl the number of subdiagonals
    @param ku the number of superdiagonals
    @param fill_num default = [Some 0]
      - If [fill_num] = [None], the elements in the generated matrix are not
        initialized;
      - If [fill_num] = [Some c], the elements in the generated matrix are
        initialized by [c].

    @since 0.2.0
 *)

val syband_dyn : 'kd Size.t ->
                 ?up:bool ->
                 ?b:(('n, 'kd) Size.syband, 'n, 'b_cd) mat ->
                 ('n, 'n, 'a_cd) mat ->
                 (('n, 'kd) Size.syband, 'n, 'b_cd) mat
(** [syband_dyn kd ?b a] converts matrix [a] into a matrix stored in
    symmetric or Hermitian band storage.
    @return matrix [b], which is overwritten.
    @param kd the number of subdiagonals or superdiagonals
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is used;
      - If [up] = [false], then the lower triangular part of [a] is used.

    @raise Invalid_arg if [kd >= dim1 a].
    @since 0.2.0
 *)

val unsyband : 'kd Size.t ->
               ?up:bool ->
               ?fill_num:num_type option ->
               ?a:('n, 'n, 'a_cd) mat ->
               (('n, 'kd) Size.syband, 'n, 'b_cd) mat ->
               ('n, 'n, 'a_cd) mat
(** [unsyband kd ?a b] converts matrix [b] stored in symmetric or Hermitian
    band storage into a matrix stored in the normal order.
    @return matrix [a], which is overwritten.
    @param kd the number of subdiagonals or superdiagonals
    @param up default = [true]
      - If [up] = [true], then [b] is treated as the upper triangular part of
        symmetric or Hermitian matrix [a];
      - If [up] = [false], then [b] is treated as the lower triangular part of
        symmetric or Hermitian matrix [a];
    @param fill_num default = [Some 0]
      - If [fill_num] = [None], the elements in the generated matrix are not
        initialized;
      - If [fill_num] = [Some c], the elements in the generated matrix are
        initialized by [c].

    @since 0.2.0
 *)

val luband_dyn : 'kl Size.t -> 'ku Size.t ->
                 ?ab:(('m, 'n, 'kl, 'ku) Size.luband, 'n, 'b_cd) mat ->
                 ('m, 'n, 'a_cd) mat ->
                 (('m, 'n, 'kl, 'ku) Size.luband, 'n, 'b_cd) mat
(** [luband_dyn kl ku ?ab a] converts matrix [a] into a matrix stored in band
    storage for LU factorization.
    @return matrix [ab], which is overwritten.
    @param kl the number of subdiagonals
    @param ku the number of superdiagonals

    @raise Invalid_arg if [kl >= dim1 a] or [ku >= dim2 a].
    @since 0.2.0
 *)

val unluband : 'm Size.t -> 'kl Size.t -> 'ku Size.t ->
               ?fill_num:num_type option ->
               ?a:('m, 'n, 'a_cd) mat ->
               (('m, 'n, 'kl, 'ku) Size.luband, 'n, 'b_cd) mat ->
               ('m, 'n, 'a_cd) mat
(** [unluband m kl ku ?a ab] converts matrix [ab] stored in band storage for LU
    factorization into a matrix stored in the normal order.
    @return matrix [a], which is overwritten.
    @param m the number of rows in [a]
    @param kl the number of subdiagonals
    @param ku the number of superdiagonals
    @param fill_num default = [Some 0]
      - If [fill_num] = [None], the elements in the generated matrix are not
        initialized;
      - If [fill_num] = [Some c], the elements in the generated matrix are
        initialized by [c].

    @since 0.2.0
 *)

(** {2 Arithmetic operations} *)

val add_const : num_type ->
                ?b:('m, 'n, 'b_cd) mat ->
                ('m, 'n, 'a_cd) mat -> ('m, 'n, 'b_cd) mat
(** [add_const c ?b a] adds constant value [c] to all elements in [a].
    @return the matrix [b], which is overwritten.
    @since 0.1.0
 *)

val sum : ('m, 'n, 'cd) mat -> num_type
(** [sum a] returns the sum of all elements in [a].
    @since 0.1.0
 *)

val trace : ('m, 'n, 'cd) mat -> num_type
(** [trace a]
    @return the sum of diagonal elements of the matrix [a].
 *)

val scal : num_type -> ('m, 'n, 'cd) mat -> unit
(** [scal alpha a] computes [a := alpha * a] with the scalar value [alpha] and
    the matrix [a].
 *)

val scal_cols : ('m, 'n, 'cd) mat -> ('n, cnt) vec -> unit
(** A column-wise [scal] function for matrices. *)

val scal_rows : ('m, cnt) vec -> ('m, 'n, 'cd) mat -> unit
(** A row-wise [scal] function for matrices. *)

val axpy : ?alpha:num_type ->
           x:('m, 'n, 'x_cd) mat ->
           ('m, 'n, 'y_cd) mat -> unit
(** [axpy ?alpha ~x y] computes [y := alpha * x + y]. *)

val gemm_diag : ?beta:num_type ->
                ?y:('n, cnt) vec ->
                transa:(('a_n, 'a_k, 'a_cd) mat ->
                        ('n, 'k, 'a_cd) mat) trans3 ->
                ?alpha:num_type ->
                ('a_n, 'a_k, 'a_cd) mat ->
                transb:(('b_k, 'b_n, 'b_cd) mat ->
                        ('k, 'n, 'b_cd) mat) trans3 ->
                ('b_k, 'b_n, 'b_cd) mat -> ('n, 'cnt) vec
(** [gemm_diag ?beta ?y ~transa ?alpha a ~transb b] executes
    [y := DIAG(alpha * OP(a) * OP(b)) + beta * y] where
    [DIAG(x)] is the vector of the diagonal elements in matrix [x], and
    [OP(x)] is one of [OP(x)] = [x], [OP(x)] = [x^T], or [OP(x)] = [x^H]
    (the conjugate transpose of [x]).

    @return the vector [y], which is overwritten.
    @param beta default = [0.0]
    @param transa the transpose flag for [a]:
       - if {!Slap.Common.normal}, then [OP(a)] = [a];
       - if {!Slap.Common.trans}, then [OP(a)] = [a^T];
       - if {!Slap.Common.conjtr}, then [OP(a)] = [a^H].
    @param transb the transpose flag for [b]:
       - if {!Slap.Common.normal}, then [OP(b)] = [b];
       - if {!Slap.Common.trans}, then [OP(b)] = [b^T];
       - if {!Slap.Common.conjtr}, then [OP(b)] = [b^H].
    @param alpha default = [1.0]

    @since 0.1.0
 *)

val syrk_diag : ?beta:num_type ->
                ?y:('n, cnt) vec ->
                trans:(('a_n, 'a_k, 'a_cd) mat ->
                       ('n, 'k, 'a_cd) mat) Common.trans2 ->
                ?alpha:num_type ->
                ('a_n, 'a_k, 'a_cd) mat -> ('n, 'cnt) vec
(** [syrk_diag ?beta ?y ~transa ?alpha a] executes

    - [y := alpha * DIAG(a * a^T) + beta * y]
      (if [trans] = {!Slap.Common.normal}) or
    - [y := alpha * DIAG(a^T * a) + beta * y]
      (if [trans] = {!Slap.Common.trans})

    where [DIAG(x)] is the vector of the diagonal elements in matrix [x].

    @return the vector [y], which is overwritten.
    @param beta default = [0.0]
    @param transa the transpose flag for [a].
    @param alpha default = [1.0]
 *)

val gemm_trace : transa:(('a_n, 'a_k, 'a_cd) mat ->
                         ('n, 'k, 'a_cd) mat) trans3 ->
                 ('a_n, 'a_k, 'a_cd) mat ->
                 transb:(('b_k, 'b_n, 'b_cd) mat ->
                         ('k, 'n, 'b_cd) mat) trans3 ->
                 ('b_k, 'b_n, 'b_cd) mat -> num_type
(** [gemm_trace ~transa a ~transb b]

    @return the trace of [OP(a) * OP(b)].
    @param transa the transpose flag for [a]:
       - if {!Slap.Common.normal}, then [OP(a)] = [a];
       - if {!Slap.Common.trans}, then [OP(a)] = [a^T];
       - if {!Slap.Common.conjtr}, then [OP(a)] = [a^H].
    @param transb the transpose flag for [b]:
       - if {!Slap.Common.normal}, then [OP(b)] = [b];
       - if {!Slap.Common.trans}, then [OP(b)] = [b^T];
       - if {!Slap.Common.conjtr}, then [OP(b)] = [b^H].
 *)

val syrk_trace : ('n, 'k, 'cd) mat -> num_type
(** [syrk_trace a] computes the trace of [a * a^T] or [a^T * a].
    @since 0.1.0
 *)

val symm2_trace : ?upa:bool ->
                  ('n, 'n, 'a_cd) mat ->
                  ?upb:bool ->
                  ('n, 'n, 'b_cd) mat -> num_type
(** [symm2_trace ?upa a ?upb b] computes the trace of [a * b] with symmetric
    matrices [a] and [b].

    @param upa default = [true]
      - If [upa] = [true], then the upper triangular part of [a] is used;
      - If [upa] = [false], then the lower triangular part of [a] is used.
    @param upb default = [true]
      - If [upb] = [true], then the upper triangular part of [b] is used;
      - If [upb] = [false], then the lower triangular part of [b] is used.
 *)

(** {2 Submatrices} *)

val submat_dyn : 'm Size.t ->
                 'n Size.t ->
                 ?ar:int ->
                 ?ac:int ->
                 (_, _, 'cd) mat ->
                 ('m, 'n, dsc) mat
(** [submat_dyn m n ?ar ?ac a]
    @return a [m]-by-[n] submatrix of the matrix [a].
    The [(i,j)] element of the returned matrix refers the [(ar+i-1,ac+j-1)]
    element of [a]. The data are shared.
    @param ar default = 1
    @param ac default = 1
 *)
