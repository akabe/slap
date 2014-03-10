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

(** The signature of {!Slap.Common}. *)

module type S =
sig
  (* implementation: slap_common_impl.ml *)

  (** {2 Size representation of vectors and matrices}
   The following types represent free algebraic operations on natural numbers.
   *)

  type +'n size = private int
  (** Singleton type on non-negative integers. *)

  type z              (** zero *)
  type +'n s          (** successor *)
  type (+'m, +'n) add (** addition of two sizes *)
  type (+'m, +'n) sub (** difference of two sizes *)
  type (+'m, +'n) mul (** multiplication of two sizes *)
  type (+'m, +'n) div (** ['m / 'n] *)
  type (+'m, +'n) min (** minimum of two sizes *)
  type (+'m, +'n) max (** maximum of two sizes *)

  type +'n packed
  (** Packed storage size of [n]-by-[n] matrix.
   It is [n*(n+1)/2].
   @see <http://www.netlib.org/lapack/lug/node123.html> Packed Storage (NetLib)
   *)

  (** {2 Data structures} *)

  type cnt

  type dsc

  (** {3 Vectors} *)

  type (+'n, 'num, 'prec, +'cnt_or_dsc) vec
  (** [('n, 'num, 'prec, 'cnt_or_dsc) vec] is the type of ['n]-dimensional
   vector whose elements have OCaml type ['num], representation kind ['prec] and
   memory continuity ['cnt_or_dsc].
   The internal implementation is fortran-style one-dimensional big array.
   *)

  type (+'n, +'cnt_or_dsc) int_vec =
      ('n, int, Bigarray.int_elt, 'cnt_or_dsc) vec

  val create_int_vec : 'n size -> ('n, 'cnt) int_vec

  type (+'n, +'cnt_or_dsc) int32_vec =
      ('n, int32, Bigarray.int32_elt, 'cnt_or_dsc) vec

  val create_int32_vec : 'n size -> ('n, 'cnt) int32_vec

  (** {3 Matrices} *)

  type (+'m, +'n, 'num, 'prec, +'cnt_or_dsc) mat
  (** [('m, 'n, 'num, 'prec) mat] is the type of ['m]-by-['n] matrix whose
   elements have OCaml type ['num], representation kind ['prec] and
   memory continuity ['cnt_or_dsc].
   The internal implementation is fortran-style two-dimensional big array.
   *)

  (** {2 Flags} *)

  (** {3 Transpose flags} *)

  type (+'a, +'tag) trans

  type trans2_tag

  type +'a trans2 = ('a, trans2_tag) trans
  (** Types of transpose flags for real vectors or matrices.
      Values of this type are
      - {!Slap.Common.normal} and
      - {!Slap.Common.trans}.
  *)

  type trans3_tag

  type +'a trans3 = ('a, trans2_tag) trans
  (** Types of transpose flags for complex vectors or matrices.
      Values of this type are
      - {!Slap.Common.normal},
      - {!Slap.Common.trans} and
      - {!Slap.Common.conjtr}.
  *)

  val normal : (('m, 'n, 'num, 'prec, 'cd) mat -> ('m, 'n, 'num, 'prec, 'cd) mat, _) trans
  (** Non-transposed matrix. *)

  val trans : (('m, 'n, 'num, 'prec, 'cd) mat -> ('n, 'm, 'num, 'prec, 'cd) mat, _) trans
  (** Transpose of a matrix. *)

  val conjtr : (('m, 'n, 'num, 'prec, 'cd) mat -> ('n, 'm, 'num, 'prec, 'cd) mat,
                trans3_tag) trans
  (** Conjugate transpose of a matrix. *)

  (** {3 Direction of matrix multiplication} *)

  type (+'k, +'m, +'n) side
  (** [('k, 'm, 'n) side] represents a relation between left- or
      right-multiplication of two matrices and their sizes.

      Let [A] be a ['k]-by-['k] square matrix and [B] be a ['m]-by-['n]
      general matrix.
      The following flags in this type correspond to directions of
      multiplication of [A] and [B]:
      - {!Slap.Common.left} means to {e left} multiply [A] by [B],
        i.e. [AB]. In this case, ['k] should be ['m]; therefore its type is
        [('m, 'm, 'n) side].
      - {!Slap.Common.right} means to {e right} multiply [A] by [B],
        i.e. [BA]. In this case, ['k] should be ['n]; therefore its type is
        [('n, 'm, 'n) side].
  *)

  val left : ('m, 'm, 'n) side

  val right : ('n, 'm, 'n) side

  (** {3 Matrix norms} *)

  type (+'a, +'tag) norm

  type norm2_tag

  type 'a norm2 = ('a, norm2_tag) norm
  (** Values of this type are
      - {!Slap.Common.norm_1}
      - {!Slap.Common.norm_inf}
  *)

  type norm4_tag

  type 'a norm4 = ('a, norm4_tag) norm
  (** Values of this type are
      - {!Slap.Common.norm_1}
      - {!Slap.Common.norm_inf}
      - {!Slap.Common.norm_amax}
      - {!Slap.Common.norm_frob}
  *)

  type norm_1

  val norm_1 : (norm_1, _) norm
  (** 1-norm of a matrix (maximum column sum). *)

  type norm_inf

  val norm_inf : (norm_inf, _) norm
  (** Infinity-norm of a matrix (maximum row sum). *)

  type norm_amax

  val norm_amax : (norm_amax, norm4_tag) norm
  (** Largest absolute value of a matrix. (not a matrix norm) *)

  type norm_frob

  val norm_frob : (norm_frob, norm4_tag) norm
  (** Frobenius norm of a matrix. *)

  (** {3 SVD computation flags} *)

  type +'a svd_job

  type svd_all

  val svd_all : svd_all svd_job

  type svd_top

  val svd_top : svd_top svd_job

  type svd_overwrite

  val svd_overwrite : svd_overwrite svd_job

  type svd_no

  val svd_no : svd_no svd_job

  (** {3 Other flags} *)

  type diag = [ `N | `U ]
end
