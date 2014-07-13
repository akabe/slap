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

(** {2 Flags} *)

type diag = [ `N | `U ]

(** {3 Transpose flags} *)

type (+'a, +'tag) trans

type transNT

type +'a trans2 = ('a, transNT) trans
(** Types of transpose flags for real vectors or matrices.
 Values of this type are
 - {!Slap.Common.normal} and
 - {!Slap.Common.trans}.
 *)

type transNTC

type +'a trans3 = ('a, transNTC) trans
(** Types of transpose flags for complex vectors or matrices.
 Values of this type are
 - {!Slap.Common.normal},
 - {!Slap.Common.trans} and
 - {!Slap.Common.conjtr}.
 *)

val normal : (('m, 'n, 'num, 'prec, 'cd) Mat.t ->
              ('m, 'n, 'num, 'prec, 'cd) Mat.t, _) trans
(** Non-transposed matrix. *)

val trans : (('m, 'n, 'num, 'prec, 'cd) Mat.t ->
             ('n, 'm, 'num, 'prec, 'cd) Mat.t, _) trans
(** Transpose of a matrix. *)

val conjtr : (('m, 'n, 'num, 'prec, 'cd) Mat.t ->
              ('n, 'm, 'num, 'prec, 'cd) Mat.t) trans3
(** Conjugate transpose of a matrix. *)

(** {3 Direction of matrix multiplication} *)

type (+'k, +'m, +'n) side
(** [('k, 'm, 'n) side] is the type of left- and right-multiplication flags.
 The type parameters ['k], ['m] and ['n] correspond to dimensions of two
 multiplied matrices: Let [A] be a ['k]-by-['k] square matrix and [B] be
 a ['m]-by-['n] general matrix.
 - When [A] is multiplied from the left by [B] (i.e., [A*B]), ['k] is equal to
   ['m]; therefore the type of {!Slap.Common.left} is [('m, 'm, 'n) side].
 - Conversely, if [A] is right-multiplied by [B] (i.e., [B*A]), ['k] is equal to
   ['n]. Thus, the flag {!Slap.Common.right} is given the type
   [('n, 'm, 'n) side].
 *)

val left : ('m, 'm, 'n) side

val right : ('n, 'm, 'n) side

(** {3 Matrix norms} *)

type (+'a, +'tag) norm

type norm2_tag

type +'a norm2 = ('a, norm2_tag) norm
(** Values of this type are {!Slap.Common.norm_1} and {!Slap.Common.norm_inf}.
 *)

type norm4_tag

type +'a norm4 = ('a, norm4_tag) norm
(** Values of this type are
  - {!Slap.Common.norm_1},
  - {!Slap.Common.norm_inf},
  - {!Slap.Common.norm_amax} and
  - {!Slap.Common.norm_frob}.
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

type (+'a, +'b, +'c, +'d, +'e) svd_job

val svd_all : ('a, 'a, 'b, 'c, 'd) svd_job

val svd_top : ('b, 'a, 'b, 'c, 'd) svd_job

val svd_overwrite : ('c, 'a, 'b, 'c, 'd) svd_job

val svd_no : ('d, 'a, 'b, 'c, 'd) svd_job

(** {2 Integer vectors} *)

type (+'n, +'cnt_or_dsc) int_vec =
    ('n, int, Bigarray.int_elt, 'cnt_or_dsc) Vec.t

val create_int_vec : 'n Size.t -> ('n, 'cnt) int_vec

type (+'n, +'cnt_or_dsc) int32_vec =
    ('n, int32, Bigarray.int32_elt, 'cnt_or_dsc) Vec.t

val create_int32_vec : 'n Size.t -> ('n, 'cnt) int32_vec
