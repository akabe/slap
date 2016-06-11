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

(** {!Slap.Common} contains definitions independent of the precision. *)

open Bigarray

(** {2 Flags} *)

(** {3 Diagonal-element flags} *)

type diag
(** The type of diagonal-element flags *)

val unit : diag
(** A matrix is unit triagular. *)

val non_unit : diag
(** A matrix is not unit triagular. *)

val char_of_diag : diag -> char
(** Return a character of a flag (for BLAS/LAPACK). *)

(** {3 Uppper/lower (triangular matrix) flags} *)

type +'a uplo constraint 'a = [< `U | `L | `A ]
(** The type of upper/lower flags. *)

val upper : [> `U ] uplo
(** Using the upper triangular (or trapezoidal) part of a matrix. *)

val lower : [> `L ] uplo
(** Using the lower triangular (or trapezoidal) part of a matrix. *)

val upper_lower : [> `A ] uplo
(** Using both of the upper and lower triangular parts (i.e., all elements) of a
    matrix. *)

val char_of_uplo : _ uplo -> char
(** Return a character of a flag (for BLAS/LAPACK). *)

(** {3 Transpose flags} *)

type (+'indim, +'outdim, +'tag) trans constraint 'tag = [< `N | `T | `C ]
(** The type of transpose flags.
    - ['indim] and ['outdim] respectively mean pairs of dimensions of an input
      matrix and an output matrix:
      - Not transposing: ['indim] = ['outdim] = ['m * 'n], and
      - Transposing: ['indim] = ['m * 'n], ['outdim] = ['n * 'm]
        (swapped from ['indim]).
    - ['tag] is [`N] (normal, i.e. not transposing), [`T] (transpose), or
      [`C] (conjugate transpose). *)

type (+'indim, +'outdim, +'tag) trans2 = ('indim, 'outdim, 'tag) trans
    constraint 'tag = [< `N | `T ]
(** Types of transpose flags for real vectors or matrices.
    Values of this type are
    - {!Slap_common.normal} and
    - {!Slap_common.trans}. *)

type (+'indim, +'outdim, +'tag) trans3 = ('indim, 'outdim, 'tag) trans
(** Types of transpose flags for complex vectors or matrices.
    Values of this type are
    - {!Slap_common.normal},
    - {!Slap_common.trans} and
    - {!Slap_common.conjtr}. *)

val normal : ('m * 'n, 'm * 'n, [> `N ]) trans
(** Non-transposed matrix. *)

val trans : ('m * 'n, 'n * 'm, [> `T ]) trans
(** Transpose of a matrix. *)

val conjtr : ('m * 'n, 'n * 'm, [> `C ]) trans3
(** Conjugate transpose of a matrix. *)

val get_transposed_dim :
  ('m * 'n, 'k * 'l, _) trans ->
  'm Slap_size.t -> 'n Slap_size.t -> 'k Slap_size.t * 'l Slap_size.t
(** [get_transposed_dim trans m n] returns
    - [(m * n)] if [trans] is {!Slap_common.normal};
    - [(n * m)] if [trans] is {!Slap_common.trans} or {!Slap_common.conjtr}. *)

val char_of_trans : ('indim, 'outdim, 'tag) trans -> char
(** Return a character of a flag (for BLAS/LAPACK). *)

(** {3 Direction of matrix multiplication} *)

type (+'k, +'m, +'n) side
(** [('k, 'm, 'n) side] is the type of left- and right-multiplication flags.
    The type parameters ['k], ['m] and ['n] correspond to dimensions of two
    multiplied matrices: Let [A] be a ['k]-by-['k] square matrix
    and [B] be a ['m]-by-['n] general matrix.
    - When [A] is multiplied from the left by [B] (i.e., [A*B]), ['k] is equal
      to ['m]; therefore the type of {!Slap_common.left} is [('m, 'm, 'n) side].
    - Conversely, if [A] is right-multiplied by [B] (i.e., [B*A]), ['k] is equal
      to ['n]. Thus, the flag {!Slap_common.right} is given the type
      [('n, 'm, 'n) side]. *)

val left : ('m, 'm, 'n) side
(** Left multiplication *)

val right : ('n, 'm, 'n) side
(** Right multiplication *)

val check_side_dim :
  'k Slap_size.t -> 'm Slap_size.t -> 'n Slap_size.t ->
  ('k, 'm, 'n) side -> bool
(** Auxiliary function (used internally) *)

val char_of_side : ('k, 'm, 'n) side -> char
(** Return a character of a flag (for BLAS/LAPACK). *)

(** {3 Matrix norms} *)

type +'a norm constraint 'a = [< `O | `I | `M | `F ]
(** The type of matrix norms. *)

type +'a norm2 = 'a norm constraint 'a = [< `O | `I ]
(** Values of this type are {!Slap_common.norm_1} and {!Slap_common.norm_inf}.
 *)

type +'a norm4 = 'a norm
(** Values of this type are
  - {!Slap_common.norm_1},
  - {!Slap_common.norm_inf},
  - {!Slap_common.norm_amax} and
  - {!Slap_common.norm_frob}.
 *)

val norm_1 : [> `O ] norm
(** 1-norm of a matrix (maximum column sum). *)

val norm_inf : [> `I ] norm
(** Infinity-norm of a matrix (maximum row sum). *)

val norm_amax : [> `M ] norm
(** Largest absolute value of a matrix. (not a matrix norm) *)

val norm_frob : [> `F ] norm
(** Frobenius norm of a matrix. *)

val char_of_norm : _ norm -> char
(** Return a character of a flag (for BLAS/LAPACK). *)

(** {3 SVD computation flags} *)

type (+'a, +'b, +'c, +'d, +'e) svd_job

val svd_all : ('a, 'a, 'b, 'c, 'd) svd_job

val svd_top : ('b, 'a, 'b, 'c, 'd) svd_job

val svd_overwrite : ('c, 'a, 'b, 'c, 'd) svd_job

val svd_no : ('d, 'a, 'b, 'c, 'd) svd_job

val char_of_svd_job : (_, _, _, _, _) svd_job -> char
(** Return a character of a flag (for BLAS/LAPACK). *)

(** {2 Integer vectors} *)

type (+'n, +'cnt_or_dsc) int_vec = ('n, int, int_elt, 'cnt_or_dsc) Slap_vec.t

val create_int_vec : 'n Slap_size.t -> ('n, 'cnt) int_vec

type (+'n, +'cnt_or_dsc) int32_vec =
  ('n, int32, int32_elt, 'cnt_or_dsc) Slap_vec.t

val create_int32_vec : 'n Slap_size.t -> ('n, 'cnt) int32_vec

(** {2 Utilities} *)

val lacaml_trans2 : (_, _, _) trans -> Lacaml.Common.trans2

val lacaml_trans3 : (_, _, _) trans -> [ `N | `T | `C ]

val lacaml_svd_job : (_, _, _, _, _) svd_job -> Lacaml.Common.svd_job

(**/**)

(** {2 Internal functions} *)

val __unexpose_uplo : char -> _ uplo

val __unexpose_norm : char -> _ norm

val __unexpose_side : char -> (_, _, _) side

val __unexpose_svd_job : char -> (_, _, _, _, _) svd_job
