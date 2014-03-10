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

(* interface: slap_common.ml *)

open Bigarray

(** {2 Size representation of vectors and matrices} *)

type 'n size = int

type z
type 'n s
type ('m, 'n) add
type ('m, 'n) mul
type ('m, 'n) min
type ('m, 'n) max

type 'n packed

(** {2 Data structures} *)

type cnt

type dsc

(** {3 Vectors} *)

type ('n, 'num, 'prec, 'cnt_or_dsc) vec =
    int   (* the number of elements in a vector (>= 0) *)
    * int (* offset (>= 1) *)
    * int (* incrementation *)
    * ('num, 'prec, fortran_layout) Array1.t

(** {3 Integer vectors} *)

type ('n, 'cnt_or_dsc) int_vec =
    ('n, int, int_elt, 'cnt_or_dsc) vec

let create_int_vec n =
  let x = Array1.create int fortran_layout n in
  (n, 1, 1, x)

type ('n, 'cnt_or_dsc) int32_vec =
    ('n, int32, int32_elt, 'cnt_or_dsc) vec

let create_int32_vec n =
  let x = Array1.create int32 fortran_layout n in
  (n, 1, 1, x)

(** {3 Matrices} *)

type ('m, 'n, 'num, 'prec, +'cnt_or_dsc) mat =
    int   (* the number of rows in a matrix    (>= 0) *)
    * int (* the number of columns in a matrix (>= 0) *)
    * int (* offset of rows    (>= 1) *)
    * int (* offset of columns (>= 1) *)
    * ('num, 'prec, fortran_layout) Array2.t

(** {2 Flags} *)

(** {3 Transpose flags} *)

type ('a, 'tag) trans = [ `N | `T | `C ]

type trans2_tag

type 'a trans2 = ('a, trans2_tag) trans

type trans3_tag

type 'a trans3 = ('a, trans3_tag) trans

let normal = `N

let trans = `T

let conjtr = `C

(** {3 Direction of multiplication of matrices} *)

type ('a, 'b, 'c) side = [ `L | `R ]

let left = `L

let right = `R

(** {3 Matrix norms} *)

type ('a, 'tag) norm = [ `O | `I | `M | `F ]

type norm2_tag

type 'a norm2 = ('a, norm2_tag) norm

type norm4_tag

type 'a norm4 = ('a, norm4_tag) norm

type norm_1

let norm_1 = `O

type norm_inf

let norm_inf = `I

type norm_amax

let norm_amax = `M

type norm_frob

let norm_frob = `F

(** {3 SVD computation flags} *)

type 'a svd_job = [ `A | `S | `O | `N ]

type svd_all

let svd_all = `A

type svd_top

let svd_top = `S

type svd_overwrite

let svd_overwrite = `O

type svd_no

let svd_no = `N
