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

open Bigarray

type +'n size = int

type (+'n, 'num, 'prec, +'cnt_or_dsc) vec =
    int   (* the number of elements in a vector (>= 0) *)
    * int (* an offset (>= 1) *)
    * int (* an incrementation *)
    * ('num, 'prec, fortran_layout) Array1.t

type (+'m, +'n, 'num, 'prec, +'cnt_or_dsc) mat =
    int   (* the number of rows in a matrix    (>= 0) *)
    * int (* the number of columns in a matrix (>= 0) *)
    * int (* an offset of rows    (>= 1) *)
    * int (* an offset of columns (>= 1) *)
    * ('num, 'prec, fortran_layout) Array2.t

type cnt

type dsc

(** {2 Flags} *)

type diag = [ `N | `U ]

(** {3 Transpose flags} *)

type (+'a, +'tag) trans = [ `N | `T | `C ]

type transNT

type +'a trans2 = ('a, transNT) trans

type transNTC

type +'a trans3 = ('a, transNTC) trans

let normal = `N

let trans = `T

let conjtr = `C

(** {3 Direction of multiplication of matrices} *)

type (+'k, +'m, +'n) side = [ `L | `R ]

let left = `L

let right = `R

(** {3 Matrix norms} *)

type (+'a, +'tag) norm = [ `O | `I | `M | `F ]

type norm2_tag

type +'a norm2 = ('a, norm2_tag) norm

type norm4_tag

type +'a norm4 = ('a, norm4_tag) norm

type norm_1

let norm_1 = `O

type norm_inf

let norm_inf = `I

type norm_amax

let norm_amax = `M

type norm_frob

let norm_frob = `F

(** {3 SVD computation flags} *)

type (+'a, +'b, +'c, +'d, +'e) svd_job = [ `A | `S | `O | `N ]

let svd_all = `A

let svd_top = `S

let svd_overwrite = `O

let svd_no = `N

(** {2 Integer vectors} *)

type (+'n, +'cnt_or_dsc) int_vec =
    ('n, int, int_elt, 'cnt_or_dsc) vec

let create_int_vec n = (n, 1, 1, Array1.create int fortran_layout n)

type (+'n, +'cnt_or_dsc) int32_vec =
    ('n, int32, int32_elt, 'cnt_or_dsc) vec

let create_int32_vec n = (n, 1, 1, Array1.create int32 fortran_layout n)

(** {2 Utilities} *)

let id x = x

let (|>) x f = f x (* for OCaml 4.00 or below *)

let get_transposed_dim t m n =
  match t with
  | `N -> (m, n)
  | _ -> (n, m)

let lacaml_trans2 = function
  | `N -> `N
  | `T | `C -> `T

let lacaml_trans3 = id

let lacaml_side = id

let lacaml_norm2 v : Lacaml.Common.norm2 =
  match v with
  | `O -> `O
  | `I -> `I
  | _ -> assert(false)

let lacaml_norm4 = id

let lacaml_norm2_opt = function
  | None -> None
  | Some v -> Some (lacaml_norm2 v)

let lacaml_norm4_opt = function
  | None -> None
  | Some v -> Some (lacaml_norm4 v)

let lacaml_svd_job = id

(** {2 Internal functions} *)

let __expose_size = id
let __unexpose_size = id
let __expose_vec = id
let __unexpose_vec = id
let __expose_mat = id
let __unexpose_mat = id

let check_side_dim m n k = function
  | `L -> m = k
  | `R -> n = k
