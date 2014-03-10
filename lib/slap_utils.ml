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

(** The signature of {!Slap.Utils}. *)

module type S =
sig
  (* implementation: slap_utils_impl.ml *)

  module Common : Slap_common.S (** = {!Slap.Common} *)

  (** {2 Numerical utilities} *)

  val cmp_float : ?epsilon:float -> float -> float -> bool
  (** [cmp_float ?epsilon x y]
      @return [true] if the difference between [x] and [y] is smaller than
      [epsilon].
      @param epsilon default = [1e-6].
  *)

  val cmp_complex : ?epsilon:float -> Complex.t -> Complex.t -> bool
  (** [cmp_complex ?epsilon x y]
      @return [true] if the difference between [x] and [y] is smaller than
      [epsilon].
      @param epsilon default = [1e-6].
  *)

  (** {2 Matrix dimensions} *)

  val dim_array_array : 'a array array -> (int * int) option
  (** [dim_array_array aa]
   @return [Some (n_rows, n_cols)] with the number of rows [n_rows] and the
   number of columns [n_cols] of the given array of arrays. If [aa] is not
   rectangular, [None] is returned.
  *)

  val dim_list_list : 'a list list -> (int * int) option
  (** [dim_list_list ll]
   @return [Some (n_rows, n_cols)] with the number of rows [n_rows] and the
   number of columns [n_cols] of the given list of lists. If [ll] is not
   rectangular, [None] is returned.
  *)
end
