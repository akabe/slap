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

(** {!Slap_common} contains definitions independent of the precision. *)

open Bigarray

type +'n size = private int
(** A singleton type on sizes (i.e., dimensions of vectors and matrices).

    Evaluation of a term with {i singleton type} ['n size] {b always} results
    in the natural number corresponding to phantom type parameter ['n].
    ['n] is instantiated to a generative phantom type or a (phantom) type that
    represents an arithmetic operation defined in this module. In either case,
    {b only} the equality of sizes is verified statically.
 *)

type (+'n, 'num, 'prec, +'cnt_or_dsc) vec
(** [('n, 'num, 'prec, 'cnt_or_dsc) vec] is the type of ['n]-dimensional vector
    whose elements have OCaml type ['num], representation kind ['prec] and
    memory contiguity flag ['cnt_or_dsc].
    The internal implementation is fortran-style one-dimensional big array.
*)

type (+'m, +'n, 'num, 'prec, +'cnt_or_dsc) mat
(** [('m, 'n, 'num, 'prec) mat] is the type of ['m]-by-['n] matrix whose
    elements have OCaml type ['num], representation kind ['prec] and memory
    contiguity ['cnt_or_dsc].
    The internal implementation is fortran-style two-dimensional big array.
*)

type cnt
(** The tag for contiguous vectors and matrices. *)

type dsc
(** The tag for discrete vectors and matrices. *)

(** {2 Flags} *)

type diag = [ `N | `U ]

(** {3 Transpose flags} *)

type (+'a, +'tag) trans

type transNT

type +'a trans2 = ('a, transNT) trans
(** Types of transpose flags for real vectors or matrices.
 Values of this type are
 - {!Slap_common.normal} and
 - {!Slap_common.trans}.
 *)

type transNTC

type +'a trans3 = ('a, transNTC) trans
(** Types of transpose flags for complex vectors or matrices.
 Values of this type are
 - {!Slap_common.normal},
 - {!Slap_common.trans} and
 - {!Slap_common.conjtr}.
 *)

val normal : (('m, 'n, 'num, 'prec, 'cd) mat ->
              ('m, 'n, 'num, 'prec, 'cd) mat, _) trans
(** Non-transposed matrix. *)

val trans : (('m, 'n, 'num, 'prec, 'cd) mat ->
             ('n, 'm, 'num, 'prec, 'cd) mat, _) trans
(** Transpose of a matrix. *)

val conjtr : (('m, 'n, 'num, 'prec, 'cd) mat ->
              ('n, 'm, 'num, 'prec, 'cd) mat) trans3
(** Conjugate transpose of a matrix. *)

(** {3 Direction of matrix multiplication} *)

type (+'k, +'m, +'n) side
(** [('k, 'm, 'n) side] is the type of left- and right-multiplication flags.
 The type parameters ['k], ['m] and ['n] correspond to dimensions of two
 multiplied matrices: Let [A] be a ['k]-by-['k] square matrix and [B] be
 a ['m]-by-['n] general matrix.
 - When [A] is multiplied from the left by [B] (i.e., [A*B]), ['k] is equal to
   ['m]; therefore the type of {!Slap_common.left} is [('m, 'm, 'n) side].
 - Conversely, if [A] is right-multiplied by [B] (i.e., [B*A]), ['k] is equal to
   ['n]. Thus, the flag {!Slap_common.right} is given the type
   [('n, 'm, 'n) side].
 *)

val left : ('m, 'm, 'n) side

val right : ('n, 'm, 'n) side

(** {3 Matrix norms} *)

type (+'a, +'tag) norm

type norm2_tag

type +'a norm2 = ('a, norm2_tag) norm
(** Values of this type are {!Slap_common.norm_1} and {!Slap_common.norm_inf}.
 *)

type norm4_tag

type +'a norm4 = ('a, norm4_tag) norm
(** Values of this type are
  - {!Slap_common.norm_1},
  - {!Slap_common.norm_inf},
  - {!Slap_common.norm_amax} and
  - {!Slap_common.norm_frob}.
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
    ('n, int, Bigarray.int_elt, 'cnt_or_dsc) vec

val create_int_vec : 'n size -> ('n, 'cnt) int_vec

type (+'n, +'cnt_or_dsc) int32_vec =
    ('n, int32, Bigarray.int32_elt, 'cnt_or_dsc) vec

val create_int32_vec : 'n size -> ('n, 'cnt) int32_vec

(** {2 Utilities} *)

val get_transposed_dim :
  (('m, 'n, 'num, 'prec, _) mat ->
   ('k, 'l, 'num, 'prec, _) mat, _) trans ->
  'm size -> 'n size -> 'k size * 'l size
(** [get_transposed_dim trans m n] returns
    - [(m * n)] if [trans] is {!Slap_common.normal};
    - [(n * m)] if [trans] is {!Slap_common.trans} or {!Slap_common.conjtr}.
*)

val lacaml_trans2 : (_, _) trans -> Lacaml.Common.trans2

val lacaml_trans3 : (_, _) trans -> [ `N | `T | `C ]

val lacaml_side : (_, _, _) side -> Lacaml.Common.side

val lacaml_norm2 : (_, _) norm -> Lacaml.Common.norm2

val lacaml_norm4 : (_, _) norm -> Lacaml.Common.norm4

val lacaml_norm2_opt : (_, _) norm option -> Lacaml.Common.norm2 option

val lacaml_norm4_opt : (_, _) norm option -> Lacaml.Common.norm4 option

val lacaml_svd_job : (_, _, _, _, _) svd_job -> Lacaml.Common.svd_job

(**/**)

(** {2 Internal functions} *)

val __expose_size : 'n size -> int

val __unexpose_size : int -> 'n size

val __expose_vec :
  ('n, 'num, 'prec, 'cnt_or_dsc) vec ->
  'n size * int * int * ('num, 'prec, fortran_layout) Array1.t

val __unexpose_vec :
  'n size * int * int * ('num, 'prec, fortran_layout) Array1.t ->
  ('n, 'num, 'prec, 'cnt_or_dsc) vec

val __expose_mat :
  ('m, 'n, 'num, 'prec, 'cnt_or_dsc) mat ->
  'm size * 'n size * int * int * ('num, 'prec, fortran_layout) Array2.t

val __unexpose_mat :
  'm size * 'n size * int * int * ('num, 'prec, fortran_layout) Array2.t ->
  ('m, 'n, 'num, 'prec, 'cnt_or_dsc) mat

val check_side_dim :
  'k size -> 'm size -> 'n size -> ('k, 'm, 'n) side -> bool
