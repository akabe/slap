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
open Slap_misc

module S = Slap_size

(** {2 Flags} *)

type diag = char

let unit = 'U'

let non_unit = 'N'

(** {3 Uppper/lower (triangular matrix) flags} *)

type +'a uplo = char constraint 'a = [< `U | `L | `A ]

let upper = 'U'

let lower = 'L'

let upper_lower = 'A'

(** {3 Transpose flags} *)

type (+'a, +'tag) trans = char

type transNT

type +'a trans2 = ('a, transNT) trans

type transNTC

type +'a trans3 = ('a, transNTC) trans

let normal = 'N'

let trans = 'T'

let conjtr = 'C'

(** {3 Direction of multiplication of matrices} *)

type (+'k, +'m, +'n) side = char

let left = 'L'

let right = 'R'

(** {3 Matrix norms} *)

type (+'a, +'tag) norm = char

type norm2_tag

type +'a norm2 = ('a, norm2_tag) norm

type norm4_tag

type +'a norm4 = ('a, norm4_tag) norm

type norm_1

let norm_1 = 'O'

type norm_inf

let norm_inf = 'I'

type norm_amax

let norm_amax = 'M'

type norm_frob

let norm_frob = 'F'

(** {3 SVD computation flags} *)

type (+'a, +'b, +'c, +'d, +'e) svd_job = char

let svd_all = 'A'

let svd_top = 'S'

let svd_overwrite = 'O'

let svd_no = 'N'

(** {2 Integer vectors} *)

type (+'n, +'cnt_or_dsc) int_vec =
    ('n, int, int_elt, 'cnt_or_dsc) Slap_vec.t

let create_int_vec n = Slap_vec.create int n

type (+'n, +'cnt_or_dsc) int32_vec =
    ('n, int32, int32_elt, 'cnt_or_dsc) Slap_vec.t

let create_int32_vec n = Slap_vec.create int32 n

(** {2 Utilities} *)

let get_transposed_dim t m n =
  let retype n = S.__unexpose (S.__expose n) in
  match t with
  | 'N' -> (retype m, retype n)
  | _ -> (retype n, retype m)

let lacaml_trans2 = function
  | 'N' -> `N
  | 'T' | 'C' -> `T
  | _ -> assert false

let lacaml_trans3 = function
  | 'N' -> `N
  | 'T' -> `T
  | 'C' -> `C
  | _ -> assert false

let lacaml_side = function
  | 'L' -> `L
  | _ -> `R

let lacaml_norm2 v : Lacaml.Common.norm2 =
  match v with
  | 'O' -> `O
  | 'I' -> `I
  | _ -> assert(false)

let lacaml_norm4 v : Lacaml.Common.norm4 =
  match v with
  | 'O' -> `O
  | 'I' -> `I
  | 'M' -> `M
  | 'F' -> `F
  | _ -> assert(false)

let lacaml_norm2_opt = function
  | None -> None
  | Some v -> Some (lacaml_norm2 v)

let lacaml_norm4_opt = function
  | None -> None
  | Some v -> Some (lacaml_norm4 v)

let lacaml_svd_job = function
  | 'A' -> `A
  | 'S' -> `S
  | 'O' -> `O
  | 'N' -> `N
  | _ -> assert(false)

let lacaml_diag = function
  | 'N' -> `N
  | 'U' -> `U
  | _ -> assert(false)

(** {2 Internal functions} *)

let __expose_uplo = identity

let __unexpose_uplo = identity

let __expose_norm = identity

let __unexpose_norm = identity

let __expose_side = identity

let __unexpose_side = identity

let check_side_dim k m n = function
  | 'L' -> S.__expose m = S.__expose k
  | _ -> S.__expose n = S.__expose k
