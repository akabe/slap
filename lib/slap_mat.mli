(* Sized Linear Algebra Package (SLAP)

  Copyright (C) 2013- Akinori ABE <abe@kb.ecei.tohoku.ac.jp>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** {!Slap.Mat} provides operations on sized matrices. *)

open Bigarray
open Slap_common

type (+'m, +'n, 'num, 'prec, +'cnt_or_dsc) t =
  ('m, 'n, 'num, 'prec, 'cnt_or_dsc) mat

val cnt : ('m, 'n, 'num, 'prec, cnt) t -> ('m, 'n, 'num, 'prec, 'cnt) t
(** Recover polymorphism of the fifth type parameter. *)

(** {2 Creation of matrices} *)

val create :
  ('num, 'prec) kind ->
  'm Slap_size.t ->
  'n Slap_size.t ->
  ('m, 'n, 'num, 'prec, 'cnt) t
(** [create kind m n]
    @return a fresh [m]-by-[n] matrix (not initialized).
 *)

val make :
  ('num, 'prec) kind ->
  'm Slap_size.t ->
  'n Slap_size.t ->
  'num ->
  ('m, 'n, 'num, 'prec, 'cnt) t
(** [make kind m n x]
    @return a fresh [m]-by-[n] matrix initialized with [x].
 *)

val init :
  ('num, 'prec) kind ->
  'm Slap_size.t ->
  'n Slap_size.t ->
  (int -> int -> 'num) ->
  ('m, 'n, 'num, 'prec, 'cnt) t
(** An alias of [init_cols]. *)

val init_cols :
  ('num, 'prec) kind ->
  'm Slap_size.t ->
  'n Slap_size.t ->
  (int -> int -> 'num) ->
  ('m, 'n, 'num, 'prec, 'cnt) t
(** [init_cols kind m n f] returns a fresh [m]-by-[n] matrix whose
    the [(i,j)] element is initialized by the result of calling [f i j].
    The elements are passed column-wise.
 *)

val init_rows :
  ('num, 'prec) kind ->
  'm Slap_size.t ->
  'n Slap_size.t ->
  (int -> int -> 'num) ->
  ('m, 'n, 'num, 'prec, 'cnt) t
(** [init_rows kind m n f] returns a fresh [m]-by-[n] matrix whose
    the [(i,j)] element is initialized by the result of calling [f i j].
    The elements are passed row-wise.
 *)

(** {2 Accessors} *)

val kind : ('m, 'n, 'num, 'prec, 'cd) t -> ('num, 'prec) kind
(** @return the kind of the given big array. *)

val dim : ('m, 'n, 'num, 'prec, 'cd) t -> 'm size * 'n size
(** [dim a] is [(dim1 a, dim2 a)]. *)

val dim1 : ('m, 'n, 'num, 'prec, 'cd) t -> 'm size
(** [dim1 a]
    @return the number of rows in [a].
 *)

val dim2 : ('m, 'n, 'num, 'prec, 'cd) t -> 'n size
(** [dim1 a]
    @return the number of columns in [a].
 *)

val get_dyn : ('m, 'n, 'num, 'prec, 'cd) t -> int -> int -> 'num
(** [get_dyn a i j]
    @return the [(i,j)] element of the matrix [a].
 *)

val set_dyn : ('m, 'n, 'num, 'prec, 'cd) t -> int -> int -> 'num -> unit
(** [set_dyn a i j x] assigns [x] to the [(i,j)] element of the matrix [a]. *)

val unsafe_get : ('m, 'n, 'num, 'prec, 'cd) t -> int -> int -> 'num
(** Like {!Slap_mat.get_dyn}, but size checking is not always performed. *)

val unsafe_set : ('m, 'n, 'num, 'prec, 'cd) t -> int -> int -> 'num -> unit
(** Like {!Slap_mat.set_dyn}, but size checking is not always performed. *)

val replace_dyn :
  ('m, 'n, 'num, 'prec, 'cd) t -> int -> int ->
  ('num -> 'num) -> unit
(** [replace_dyn a i j f] is [set a i j (f (get a i j))]. *)

val col_dyn :
  ('m, 'n, 'num, 'prec, 'cd) t ->
  int ->
  ('m, 'num, 'prec, 'cnt) Slap_vec.t
(** [col_dyn a i]
    @return the [i]-th column of the matrix [a]. The data are shared.
 *)

val row_dyn :
  ('m, 'n, 'num, 'prec, 'cd) t ->
  int ->
  ('n, 'num, 'prec, dsc) Slap_vec.t
(** [row_dyn a i]
    @return the [i]-th row of the matrix [a]. The data are shared.
 *)

val diag : ('n, 'n, 'num, 'prec, 'cd) t -> ('n, 'num, 'prec, dsc) Slap_vec.t
(** [diag a]
    @return the diagonal elements of the square matrix [a]. The data are shared.
*)

val diag_rect :
  ('m, 'n, 'num, 'prec, 'cd) t ->
  (('m, 'n) Slap_size.min, 'num, 'prec, dsc) Slap_vec.t
(** [diag_rect a]
    @return the diagonal elements of the rectangular matrix [a].
    The data are shared.
    @since 1.0.0
 *)

val as_vec :
  ('m, 'n, 'num, 'prec, cnt) t ->
  (('m, 'n) Slap_size.mul, 'num, 'prec, 'cnt) Slap_vec.t
(** [as_vec a]
    @return the vector containing all elements of the matrix in column-major
    order. The data are shared.
 *)

(** {2 Basic operations} *)

val fill : ('m, 'n, 'num, 'prec, 'cd) t -> 'num -> unit
(** Fill the given matrix with the given value. *)

val copy :
  ?b:('m, 'n, 'num, 'prec, 'b_cd) t ->
  ('m, 'n, 'num, 'prec, 'a_cd) t ->
  ('m, 'n, 'num, 'prec, 'b_cd) t
(** [copy ?b a] copies the matrix [a] into the matrix [b].
    @return [b], which is overwritten.
    @param b default = a fresh matrix.
 *)

(** {2 Matrix transformations} *)

val packed :
  ?up:bool ->
  ?x:('n Slap_size.packed, 'num, 'prec, cnt) Slap_vec.t ->
  ('n, 'n, 'num, 'prec, 'cd) t ->
  ('n Slap_size.packed, 'num, 'prec, 'cnt) Slap_vec.t
(** [packed ?up ?x a] transforms triangular matrix [a] into packed storage
    format.
    @return vector [x], which is overwritten.
    @param up default = [true]
      - If [up] = [true], then the upper triangular part of [a] is packed;
      - If [up] = [false], then the lower triangular part of [a] is packed.
    @since 0.2.0
 *)

val unpacked :
  ?up:bool ->
  ?fill_num:'num option ->
  ?a:('n, 'n, 'num, 'prec, 'cd) t ->
  ('n Slap_size.packed, 'num, 'prec, cnt) Slap_vec.t ->
  ('n, 'n, 'num, 'prec, 'cd) t
(** [unpacked ?up ?fill_num ?a x] generates an upper or lower triangular matrix
    from [x] stored in packed storage.
    @return matrix [a], which is overwritten.
    @param up default = [true]
      - If [up] = [true], then the upper triangular matrix is generated;
      - If [up] = [false], then the lower triangular matrix is generated.
    @param fill_num default = [None] ({b Note}: The default value in
      [Slap.[SDCZ].Mat.unpacked] is [Some 0], not [None].)
      - If [fill_num] = [None], the elements in the generated matrix are not
        initialized;
      - If [fill_num] = [Some c], the elements in the generated matrix are
        initialized by [c].
    @since 0.2.0
 *)

val geband_dyn :
  'kl Slap_size.t -> 'ku Slap_size.t ->
  ?b:(('m, 'n, 'kl, 'ku) Slap_size.geband, 'n, 'num, 'prec, 'b_cd) t ->
  ('m, 'n, 'num, 'prec, 'a_cd) t ->
  (('m, 'n, 'kl, 'ku) Slap_size.geband, 'n, 'num, 'prec, 'b_cd) t
(** [geband_dyn kl ku ?b a] converts matrix [a] into a matrix stored in band
    storage.
    @return matrix [b], which is overwritten.
    @param kl the number of subdiagonals
    @param ku the number of superdiagonals

    @raise Invalid_arg if [kl >= dim1 a] or [ku >= dim2 a].
    @since 0.2.0
 *)

val ungeband :
  'm Slap_size.t -> 'kl Slap_size.t -> 'ku Slap_size.t ->
  ?fill_num:'num option ->
  ?a:('m, 'n, 'num, 'prec, 'a_cd) t ->
  (('m, 'n, 'kl, 'ku) Slap_size.geband, 'n, 'num, 'prec, 'b_cd) t ->
  ('m, 'n, 'num, 'prec, 'a_cd) t
(** [ungeband m kl ku ?a b] converts matrix [b] stored in band storage into
    a matrix stored in the normal order.
    @return matrix [a], which is overwritten.
    @param m the number of rows in [a]
    @param kl the number of subdiagonals
    @param ku the number of superdiagonals
    @param fill_num default = [None] ({b Note}: The default value in
      [Slap.[SDCZ].Mat.ungeband_dyn] is [Some 0], not [None].)
      - If [fill_num] = [None], the elements in the generated matrix are not
        initialized;
      - If [fill_num] = [Some c], the elements in the generated matrix are
        initialized by [c].

    @since 0.2.0
 *)

val syband_dyn :
  'kd Slap_size.t ->
  ?up:bool ->
  ?b:(('n, 'kd) Slap_size.syband, 'n, 'num, 'prec, 'b_cd) t ->
  ('n, 'n, 'num, 'prec, 'a_cd) t ->
  (('n, 'kd) Slap_size.syband, 'n, 'num, 'prec, 'b_cd) t
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

val unsyband :
  'kd Slap_size.t ->
  ?up:bool ->
  ?fill_num:'num option ->
  ?a:('n, 'n, 'num, 'prec, 'a_cd) t ->
  (('n, 'kd) Slap_size.syband, 'n, 'num, 'prec, 'b_cd) t ->
  ('n, 'n, 'num, 'prec, 'a_cd) t
(** [unsyband kd ?a b] converts matrix [b] stored in symmetric or Hermitian
    band storage into a matrix stored in the normal order.
    @return matrix [a], which is overwritten.
    @param kd the number of subdiagonals or superdiagonals
    @param up default = [true]
      - If [up] = [true], then [b] is treated as the upper triangular part of
        symmetric or Hermitian matrix [a];
      - If [up] = [false], then [b] is treated as the lower triangular part of
        symmetric or Hermitian matrix [a];
    @param fill_num default = [None] ({b Note}: The default value in
      [Slap.[SDCZ].Mat.unsyband_dyn] is [Some 0], not [None].)
      - If [fill_num] = [None], the elements in the generated matrix are not
        initialized;
      - If [fill_num] = [Some c], the elements in the generated matrix are
        initialized by [c].

    @since 0.2.0
 *)

val luband_dyn :
  'kl Slap_size.t -> 'ku Slap_size.t ->
  ?ab:(('m, 'n, 'kl, 'ku) Slap_size.luband, 'n, 'num, 'prec, 'b_cd) t ->
  ('m, 'n, 'num, 'prec, 'a_cd) t ->
  (('m, 'n, 'kl, 'ku) Slap_size.luband, 'n, 'num, 'prec, 'b_cd) t
(** [luband_dyn kl ku ?ab a] converts matrix [a] into a matrix stored in band
    storage for LU factorization.
    @return matrix [ab], which is overwritten.
    @param kl the number of subdiagonals
    @param ku the number of superdiagonals

    @raise Invalid_arg if [kl >= dim1 a] or [ku >= dim2 a].
    @since 0.2.0
 *)

val unluband :
  'm Slap_size.t -> 'kl Slap_size.t -> 'ku Slap_size.t ->
  ?fill_num:'num option ->
  ?a:('m, 'n, 'num, 'prec, 'a_cd) t ->
  (('m, 'n, 'kl, 'ku) Slap_size.luband, 'n, 'num, 'prec, 'b_cd) t ->
  ('m, 'n, 'num, 'prec, 'a_cd) t
(** [unluband m kl ku ?a ab] converts matrix [ab] stored in band storage for LU
    factorization into a matrix stored in the normal order.
    @return matrix [a], which is overwritten.
    @param m the number of rows in [a]
    @param kl the number of subdiagonals
    @param ku the number of superdiagonals
    @param fill_num default = [None] ({b Note}: The default value in
      [Slap.[SDCZ].Mat.unluband_dyn] is [Some 0], not [None].)
      - If [fill_num] = [None], the elements in the generated matrix are not
        initialized;
      - If [fill_num] = [Some c], the elements in the generated matrix are
        initialized by [c].

    @since 0.2.0
 *)

(** {2 Iterators} *)

val map :
  ('b_num, 'b_prec) kind ->
  ('a_num -> 'b_num) ->
  ?b:('m, 'n, 'b_num, 'b_prec, 'b_cd) t ->
  ('m, 'n, 'a_num, 'a_prec, 'a_cd) t ->
  ('m, 'n, 'b_num, 'b_prec, 'b_cd) t

val mapi :
  ('b_num, 'b_prec) kind ->
  (int -> int -> 'a_num -> 'b_num) ->
  ?b:('m, 'n, 'b_num, 'b_prec, 'b_cd) t ->
  ('m, 'n, 'a_num, 'a_prec, 'a_cd) t ->
  ('m, 'n, 'b_num, 'b_prec, 'b_cd) t

val fold_left :
  ('accum -> ('m, 'num, 'prec, 'x_cd) Slap_vec.t -> 'accum) ->
  'accum ->
  ('m, 'n, 'num, 'prec, 'a_cd) t -> 'accum
(** [fold_left f init a] folds column vectors of matrix [a] by [f] in the order
    left to right.
 *)

val fold_lefti :
  (int -> 'accum -> ('m, 'num, 'prec, 'x_cd) Slap_vec.t -> 'accum) ->
  'accum ->
  ('m, 'n, 'num, 'prec, 'a_cd) t -> 'accum
(** [fold_lefti f init a] folds column vectors of matrix [a] by [f] in the order
    left to right.
 *)

val fold_right :
  (('m, 'num, 'prec, 'x_cd) Slap_vec.t -> 'accum -> 'accum) ->
  ('m, 'n, 'num, 'prec, 'a_cd) t ->
  'accum -> 'accum
(** [fold_right f a init] folds column vectors of matrix [a] by [f] in the
    order right to left.
 *)

val fold_righti :
  (int -> ('m, 'num, 'prec, 'x_cd) Slap_vec.t -> 'accum -> 'accum) ->
  ('m, 'n, 'num, 'prec, 'a_cd) t ->
  'accum -> 'accum
(** [fold_righti f a init] folds column vectors of matrix [a] by [f] in the
    order right to left.
 *)

val fold_top :
  ('accum -> ('n, 'num, 'prec, dsc) Slap_vec.t -> 'accum) ->
  'accum ->
  ('m, 'n, 'num, 'prec, 'a_cd) t -> 'accum
(** [fold_top f init a] folds row vectors of matrix [a] by [f] in the order
    top to bottom.
 *)

val fold_topi :
  (int -> 'accum -> ('n, 'num, 'prec, dsc) Slap_vec.t -> 'accum) ->
  'accum ->
  ('m, 'n, 'num, 'prec, 'a_cd) t -> 'accum
(** [fold_topi f init a] folds row vectors of matrix [a] by [f] in the order
    top to bottom.
 *)

val fold_bottom :
  (('n, 'num, 'prec, dsc) Slap_vec.t -> 'accum -> 'accum) ->
  ('m, 'n, 'num, 'prec, 'a_cd) t ->
  'accum -> 'accum
(** [fold_bottom f a init] folds row vectors of matrix [a] by [f] in the
    order bottom to top.
 *)

val fold_bottomi :
  (int -> ('n, 'num, 'prec, dsc) Slap_vec.t -> 'accum -> 'accum) ->
  ('m, 'n, 'num, 'prec, 'a_cd) t ->
  'accum -> 'accum
(** [fold_bottomi f a init] folds row vectors of matrix [a] by [f] in the
    order bottom to top.
 *)

val replace_all : ('m, 'n, 'num, 'prec, 'cd) t -> ('num -> 'num) -> unit
(** [replace_all a f] modifies the matrix [a] in place
    -- the [(i,j)]-element [aij] of [a] will be set to [f aij].
 *)

val replace_alli :
  ('m, 'n, 'num, 'prec, 'cd) t ->
  (int -> int -> 'num -> 'num) -> unit
(** [replace_all a f] modifies the matrix [a] in place
    -- the [(i,j)]-element [aij] of [a] will be set to [f i j aij].
 *)

(** {2 Type conversion} *)

val to_array : ('m, 'n, 'num, 'prec, 'cd) t -> 'num array array
(** [to_array a]
    @return the array of arrays of all the elements of [a].
 *)

val of_array_dyn :
  ('num, 'prec) kind ->
  'm Slap_size.t ->
  'n Slap_size.t ->
  'num array array ->
  ('m, 'n, 'num, 'prec, 'cnt) t
(** Build a matrix initialized from the given array of arrays.
    @raise Invalid_argument the given array of arrays is not rectangular or
    its size is not [m]-by-[n].
 *)

val unsafe_of_array :
  ('num, 'prec) kind ->
  'm Slap_size.t ->
  'n Slap_size.t ->
  'num array array ->
  ('m, 'n, 'num, 'prec, 'cnt) t
(** Like [unsafe_of_array], but size checking is not always performed. *)

val to_list : ('m, 'n, 'num, 'prec, 'cd) t -> 'num list list
(** [to_list a]
    @return the list of lists of all the elements of [a].
 *)

val of_list_dyn :
  ('num, 'prec) kind ->
  'm Slap_size.t ->
  'n Slap_size.t ->
  'num list list ->
  ('m, 'n, 'num, 'prec, 'cnt) t
(** Build a matrix initialized from the given list of lists.
    @raise Invalid_argument the given list of lists is not rectangular or
    its size is not [m]-by-[n].
 *)

val unsafe_of_list :
  ('num, 'prec) kind ->
  'm Slap_size.t ->
  'n Slap_size.t ->
  'num list list ->
  ('m, 'n, 'num, 'prec, 'cnt) t
(** Like [unsafe_of_list], but size checking is not always performed. *)

(** {2 Submatrices} *)

val submat_dyn :
  'm Slap_size.t ->
  'n Slap_size.t ->
  ?ar:int ->
  ?ac:int ->
  (_, _, 'num, 'prec, 'cd) t ->
  ('m, 'n, 'num, 'prec, dsc) t
(** [submat_dyn m n ?ar ?ac a]
    @return a [m]-by-[n] submatrix of the matrix [a].
    The [(i,j)] element of the returned matrix refers the [(ar+i-1,ac+j-1)]
    element of [a]. The data are shared.
    @param ar default = 1
    @param ac default = 1
 *)

(** {2 Utilities} *)

val dim_array_array : 'a array array -> (int * int) option
(** [dim_array_array aa] returns [Some (n_rows, n_cols)] with the number of rows
    [n_rows] and the number of columns [n_cols] of the given array of arrays.
    [(0, 0)] is returned if [aa] is an empty array.
    [None] is returned if [aa] is not rectangular.
 *)

val dim_list_list : 'a list list -> (int * int) option
(** [dim_list list ll] returns [Some (n_rows, n_cols)] with the number of rows
    [n_rows] and the number of columns [n_cols] of the given list of lists.
    [(0, 0)] is returned if [ll] is an empty list.
    [None] is returned if [ll] is not rectangular.
 *)

(**/**)

(** {2 Internal functions} *)

val check_cnt : ('m, 'n, 'num, 'prec, cnt) t -> bool

val opt_mat :
  'm Slap_size.t -> 'n Slap_size.t ->
  ('m, 'n, 'num, 'prec, 'cd) t option ->
  int option * int option * ('num, 'prec, fortran_layout) Array2.t option

val opt_mat_alloc :
  ('num, 'prec) kind ->
  'm Slap_size.t -> 'n Slap_size.t ->
  ('m, 'n, 'num, 'prec, 'cd) t option ->
  int * int * ('num, 'prec, fortran_layout) Array2.t
