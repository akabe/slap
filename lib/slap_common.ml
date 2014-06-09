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

(** {2 Flags} *)

type diag = [ `N | `U ]

(** {3 Transpose flags} *)

type ('a, 'tag) trans = [ `N | `T | `C ]

type transNT

type 'a trans2 = ('a, transNT) trans

type transNTC

type 'a trans3 = ('a, transNTC) trans

let normal = `N

let trans = `T

let conjtr = `C

let get_transposed_dim t m n =
  match t with
  | `N -> (m, n)
  | _ -> (n, m)

let lacaml_trans2 = function
  | `N -> `N
  | `T | `C -> `T

(** {3 Direction of multiplication of matrices} *)

type ('k, 'm, 'n) side = [ `L | `R ]

let left = `L

let right = `R

(** {3 Matrix norms} *)

type ('a, 'tag) norm = [ `O | `I | `M | `F ]

type norm2_tag

type 'a norm2 = ('a, norm2_tag) norm

let lacaml_norm2 v : Lacaml.Common.norm2 =
  match v with
  | `O -> `O
  | `I -> `I
  | _ -> assert(false)

let lacaml_norm2_opt = function
  | None -> None
  | Some v -> Some (lacaml_norm2 v)

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

type ('a, 'b, 'c, 'd, 'e) svd_job = [ `A | `S | `O | `N ]

let svd_all = `A

let svd_top = `S

let svd_overwrite = `O

let svd_no = `N

(** {2 Integer vectors} *)

type ('n, 'cnt_or_dsc) int_vec =
    ('n, int, int_elt, 'cnt_or_dsc) Vec.t

let create_int_vec n = Vec.create int n

type ('n, 'cnt_or_dsc) int32_vec =
    ('n, int32, int32_elt, 'cnt_or_dsc) Vec.t

let create_int32_vec n = Vec.create int32 n

(*
  type +'n packed
  (** Packed storage size of [n]-by-[n] matrix.
   It is [n*(n+1)/2].
   @see <http://www.netlib.org/lapack/lug/node123.html> Packed Storage (NetLib)
   *)
 *)
