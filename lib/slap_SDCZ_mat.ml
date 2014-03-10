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

(** A part of the signature of [Slap.[SDCZ].Mat]. *)

module type S =
sig
  (* implementation: slap_SDCZ_mat_wrap.ml *)

  include Slap_SDCZ_types.S

  module type MAT =
    sig
      type m
      type n
      val value : (m, n, 'cnt) mat
    end

  (** {2 Creation of matrices} *)

  val create : 'm Common.size -> 'n Common.size -> ('m, 'n, 'cnt) mat
  (** [create m n]
   @return a fresh [m]-by-[n] matrix (not initialized).
   *)

  val make : 'm Common.size -> 'n Common.size -> num_type -> ('m, 'n, 'cnt) mat
  (** [make m n x]
   @return a fresh [m]-by-[n] matrix initialized with [x].
   *)

  val make0 : 'm Common.size -> 'n Common.size -> ('m, 'n, 'cnt) mat
  (** [make0] is an alias of [zeros]. *)

  val zeros : 'm Common.size -> 'n Common.size -> ('m, 'n, 'cnt) mat
  (** [zeros m n]
   @return a fresh [m]-by-[n] matrix initialized with [0].
   *)

  val ones : 'm Common.size -> 'n Common.size -> ('m, 'n, 'cnt) mat
  (** [ones m n]
   @return a fresh [m]-by-[n] matrix initialized with [1].
   *)

  val identity : 'n Common.size -> ('n, 'n, 'cnt) mat
  (** [identity n]
   @return a fresh [n]-by-[n] identity matrix.
   *)

  val init_cols : 'm Common.size -> 'n Common.size ->
                  f:(int -> int -> num_type) -> ('m, 'n, 'cnt) mat
  (** [init_cols m n ~f] returns a fresh [m]-by-[n] matrix whose
   the [(i,j)] element is initialized by the result of calling [f i j].
   The elements are passed by column-major order.
   *)

  val init_rows : 'm Common.size -> 'n Common.size ->
                  f:(int -> int -> num_type) -> ('m, 'n, 'cnt) mat
  (** [init_rows m n ~f] returns a fresh [m]-by-[n] matrix whose
   the [(i,j)] element is initialized by the result of calling [f i j].
   The elements are passed by row-major order.
   *)

  (** {2 Accessors} *)

  val dim : ('m, 'n, 'cd) mat -> 'm Common.size * 'n Common.size
  (** [dim a] is [(dim1 a, dim2 a)]. *)

  val dim1 : ('m, 'n, 'cd) mat -> 'm Common.size
  (** [dim1 a]
   @return the number of rows in [a].
   *)

  val dim2 : ('m, 'n, 'cd) mat -> 'n Common.size
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

  val row_dyn : ('m, 'n, 'cd) mat -> int -> ('n, Common.dsc) vec
  (** [row_dyn a i]
   @return the [i]-th row of the matrix [a]. The data are shared.
   *)

  val diag : ('m, 'n, 'cd) mat -> (('m, 'n) Common.min, Common.dsc) vec
  (** [diag a]
   @return the diagonal elements of the matrix [a]. The data are shared.
   *)

  val copy_diag : ('m, 'n, 'cd) mat -> (('m, 'n) Common.min, Common.dsc) vec
  (** [copy_diag a] is [Vec.copy (Mat.diag a)].
   @return the diagonal elements of the matrix [a].
   *)

  val as_vec : ('m, 'n, Common.cnt) mat -> (('m, 'n) Common.mul, 'cnt) vec
  (** [as_vec a]
   @return the vector containing all elements of the matrix in column-major
           order. The data are shared.
   *)

  (** {2 Basic operations} *)

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

  (** {2 Type conversion} *)

  val to_array : ('m, 'n, 'cd) mat -> num_type array array
  (** [to_array a]
   @return the array of arrays of all the elements of [a].
   *)

  val of_array_dyn : 'm Common.size -> 'n Common.size ->
                     num_type array array ->
                     ('m, 'n, 'cnt) mat
  (** Build a matrix initialized from the given array of arrays.
   @raise Invalid_argument the given array of arrays is not rectangular or
   its size is not [m]-by-[n].
   *)

  module Of_array_dyn (X : sig val value : num_type array array end) : MAT

  val to_list : ('m, 'n, 'cd) mat -> num_type list list
  (** [to_list a]
   @return the list of lists of all the elements of [a].
   *)

  val of_list_dyn : 'm Common.size -> 'n Common.size ->
                     num_type list list ->
                     ('m, 'n, 'cnt) mat
  (** Build a matrix initialized from the given list of lists.
   @raise Invalid_argument the given list of lists is not rectangular or
   its size is not [m]-by-[n].
  *)

  module Of_list_dyn (X : sig val value : num_type list list end) : MAT

  (** {2 Iterators} *)

  val map : ?b:('m, 'n, 'b_cd) mat -> f:(num_type -> num_type) ->
            ('m, 'n, 'a_cd) mat -> ('m, 'n, 'b_cd) mat

  val replace_all : ('m, 'n, 'cd) mat -> f:(num_type -> num_type) -> unit
  (** [replace_all a ~f] modifies the matrix [a] in place
   -- the [(i,j)]-element [aij] of [a] will be set to [f aij].
   *)

  val replace_alli : ('m, 'n, 'cd) mat ->
                     f:(int -> int -> num_type -> num_type) -> unit
  (** [replace_all a ~f] modifies the matrix [a] in place
   -- the [(i,j)]-element [aij] of [a] will be set to [f i j aij].
   *)

  (** {2 Arithmetic operations} *)

  val trace : ('m, 'n, 'cd) mat -> num_type
  (** [trace a]
   @return the sum of diagonal elements of the matrix [a].
   *)

  val scal : num_type -> ('m, 'n, 'cd) mat -> unit
  (** [scal alpha a] computes [a := alpha * a] with the scalar value [alpha] and
   the matrix [a].
   *)

  val scal_cols : ('m, 'n, 'cd) mat -> ('n, Common.cnt) vec -> unit
  (** A column-wise [scal] function for matrices. *)

  val scal_rows : ('m, Common.cnt) vec -> ('m, 'n, 'cd) mat -> unit
  (** A row-wise [scal] function for matrices. *)

  val axpy : ?alpha:num_type ->
             x:('m, 'n, 'x_cd) mat ->
             ('m, 'n, 'y_cd) mat -> unit
  (** [axpy ?alpha ~x y] computes [y := alpha * x + y]. *)
end
