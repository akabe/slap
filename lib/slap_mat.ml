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

(** The signature of {!Slap.Mat}. *)

module type S =
sig
  (* implementation: slap_mat_impl.ml *)

  module Common : Slap_common.S (** = {!Slap.Common} *)

  open Common

  (** {2 Creation of matrices} *)

  val create : ('num, 'prec) Bigarray.kind ->
               'm size ->
               'n size ->
               ('m, 'n, 'num, 'prec, 'cnt) mat
  (** [create kind m n]
   @return a fresh [m]-by-[n] matrix (not initialized).
   *)

  val make : ('num, 'prec) Bigarray.kind ->
             'm size ->
             'n size ->
             'num ->
             ('m, 'n, 'num, 'prec, 'cnt) mat
  (** [make kind m n x]
   @return a fresh [m]-by-[n] matrix initialized with [x].
   *)

  val init_cols : ('num, 'prec) Bigarray.kind ->
                  'm size ->
                  'n size ->
                  f:(int -> int -> 'num) ->
                  ('m, 'n, 'num, 'prec, 'cnt) mat
  (** [init_cols kind m n ~f] returns a fresh [m]-by-[n] matrix whose
   the [(i,j)] element is initialized by the result of calling [f i j].
   The elements are passed by column-major order.
   *)

  val init_rows : ('num, 'prec) Bigarray.kind ->
                  'm size ->
                  'n size ->
                  f:(int -> int -> 'num) ->
                  ('m, 'n, 'num, 'prec, 'cnt) mat
  (** [init_rows kind m n ~f] returns a fresh [m]-by-[n] matrix whose
   the [(i,j)] element is initialized by the result of calling [f i j].
   The elements are passed by row-major order.
   *)

  (** {2 Accessors} *)

  val kind : ('m, 'n, 'num, 'prec, 'cd) mat -> ('num, 'prec) Bigarray.kind
  (** @return the kind of the given big array. *)

  val dim : ('m, 'n, 'num, 'prec, 'cd) mat -> 'm size * 'n size
  (** [dim a] is [(dim1 a, dim2 a)]. *)

  val dim1 : ('m, 'n, 'num, 'prec, 'cd) mat -> 'm size
  (** [dim1 a]
   @return the number of rows in [a].
   *)

  val dim2 : ('m, 'n, 'num, 'prec, 'cd) mat -> 'n size
  (** [dim1 a]
   @return the number of columns in [a].
   *)

  val get_dyn : ('m, 'n, 'num, 'prec, 'cd) mat -> int -> int -> 'num
  (** [get_dyn a i j]
   @return the [(i,j)] element of the matrix [a].
   *)

  val set_dyn : ('m, 'n, 'num, 'prec, 'cd) mat -> int -> int -> 'num -> unit
  (** [set_dyn a i j x] assigns [x] to the [(i,j)] element of the matrix [a].
   *)

  val unsafe_get : ('m, 'n, 'num, 'prec, 'cd) mat -> int -> int -> 'num
  (** Like {!Slap.Mat.get_dyn}, but size checking is not always performed. *)

  val unsafe_set : ('m, 'n, 'num, 'prec, 'cd) mat -> int -> int -> 'num -> unit
  (** Like {!Slap.Mat.set_dyn}, but size checking is not always performed. *)

  val replace_dyn : ('m, 'n, 'num, 'prec, 'cd) mat -> int -> int ->
    f:('num -> 'num) -> unit
  (** [replace_dyn a i j ~f] is [set a i j (f (get a i j))]. *)

  val col_dyn : ('m, 'n, 'num, 'prec, 'cd) mat ->
                int ->
                ('m, 'num, 'prec, 'cnt) vec
  (** [col_dyn a i]
   @return the [i]-th column of the matrix [a]. The data are shared.
   *)

  val row_dyn : ('m, 'n, 'num, 'prec, 'cd) mat ->
                int ->
                ('n, 'num, 'prec, dsc) vec
  (** [row_dyn a i]
   @return the [i]-th row of the matrix [a]. The data are shared.
   *)

  val diag : ('n, 'n, 'num, 'prec, 'cd) mat ->
             ('n, 'num, 'prec, dsc) vec
  (** [diag a]
   @return the diagonal elements of the matrix [a]. The data are shared.
   *)

  val as_vec : ('m, 'n, 'num, 'prec, cnt) mat ->
               (('m, 'n) mul, 'num, 'prec, 'cnt) vec
  (** [as_vec a]
   @return the vector containing all elements of the matrix in column-major
           order. The data are shared.
   *)

  (** {2 Basic operations} *)

  val fill : ('m, 'n, 'num, 'prec, 'cd) mat -> 'num -> unit
  (** Fill the given matrix with the given value. *)

  val copy : ?b:('m, 'n, 'num, 'prec, 'b_cd) mat ->
             ('m, 'n, 'num, 'prec, 'a_cd) mat ->
             ('m, 'n, 'num, 'prec, 'b_cd) mat
  (** [copy ?b a] copies the matrix [a] into the matrix [b].
   @return [b], which is overwritten.
   @param b default = a fresh matrix.
   *)

  (** {2 Iterators} *)

  val replace_all : ('m, 'n, 'num, 'prec, 'cd) mat -> f:('num -> 'num) -> unit
  (** [replace_all a ~f] modifies the matrix [a] in place
   -- the [(i,j)]-element [aij] of [a] will be set to [f aij].
   *)

  val replace_alli : ('m, 'n, 'num, 'prec, 'cd) mat ->
                     f:(int -> int -> 'num -> 'num) -> unit
  (** [replace_all a ~f] modifies the matrix [a] in place
   -- the [(i,j)]-element [aij] of [a] will be set to [f i j aij].
   *)

  (** {2 Type conversion} *)

  val to_array : ('m, 'n, 'num, 'prec, 'cd) mat -> 'num array array
  (** [to_array a]
   @return the array of arrays of all the elements of [a].
   *)

  val of_array_dyn : ('num, 'prec) Bigarray.kind ->
                     'm size ->
                     'n size ->
                     'num array array ->
                     ('m, 'n, 'num, 'prec, 'cnt) mat
  (** Build a matrix initialized from the given array of arrays.
   @raise Invalid_argument the given array of arrays is not rectangular or
   its size is not [m]-by-[n].
   *)

  val to_list : ('m, 'n, 'num, 'prec, 'cd) mat -> 'num list list
  (** [to_list a]
   @return the list of lists of all the elements of [a].
   *)

  val of_list_dyn : ('num, 'prec) Bigarray.kind ->
                    'm size ->
                    'n size ->
                    'num list list ->
                    ('m, 'n, 'num, 'prec, 'cnt) mat
  (** Build a matrix initialized from the given list of lists.
   @raise Invalid_argument the given list of lists is not rectangular or
   its size is not [m]-by-[n].
  *)

  (** {2 Submatrices} *)

  val submat_dyn : 'm size ->
                   'n size ->
                   ?ar:int ->
                   ?ac:int ->
                   (_, _, 'num, 'prec, 'cd) mat ->
                   ('m, 'n, 'num, 'prec, dsc) mat
  (** [submat_dyn m n ?ar ?ac a]
      @return a [m]-by-[n] submatrix of the matrix [a].
      The [(i,j)] element of the returned matrix refers the [(ar+i-1,ac+j-1)]
      element of [a]. The data are shared.
      @param ar default = 1
      @param ac default = 1
  *)
end
