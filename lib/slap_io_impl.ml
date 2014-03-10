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

(* interface: slap_io.ml *)

module Common = Slap_common_impl
module Vec = Slap_vec_impl
module Mat = Slap_mat_impl

open Common

(** {2 Pretty-printing in standard style} *)

type ('n, 'num, 'prec, 'cnt_or_dsc) pp_vec =
    Format.formatter ->
    ('n, 'num, 'prec, 'cnt_or_dsc) vec -> unit

let pp_fvec fmt (n, ofsx, incx, x) =
  assert(Vec.check_cnt n ofsx incx x);
  Lacaml.Io.pp_fvec fmt x

let pp_cvec fmt (n, ofsx, incx, x) =
  assert(Vec.check_cnt n ofsx incx x);
  Lacaml.Io.pp_cvec fmt x

let pp_ivec fmt (n, ofsx, incx, x) =
  assert(Vec.check_cnt n ofsx incx x);
  Lacaml.Io.pp_ivec fmt x

let pp_rfvec fmt (n, ofsx, incx, x) =
  assert(Vec.check_cnt n ofsx incx x);
  Lacaml.Io.pp_rfvec fmt x

let pp_rcvec fmt (n, ofsx, incx, x) =
  assert(Vec.check_cnt n ofsx incx x);
  Lacaml.Io.pp_rcvec fmt x

let pp_rivec fmt (n, ofsx, incx, x) =
  assert(Vec.check_cnt n ofsx incx x);
  Lacaml.Io.pp_rivec fmt x

type ('m, 'n, 'num, 'prec, 'cnt_or_dsc) pp_mat =
    Format.formatter ->
    ('m, 'n, 'num, 'prec, 'cnt_or_dsc) mat -> unit

let pp_fmat fmt (m, n, ar, ac, a) =
  assert(Mat.check_cnt m n ar ac a);
  Lacaml.Io.pp_fmat fmt a

let pp_cmat fmt (m, n, ar, ac, a) =
  assert(Mat.check_cnt m n ar ac a);
  Lacaml.Io.pp_cmat fmt a

let pp_imat fmt (m, n, ar, ac, a) =
  assert(Mat.check_cnt m n ar ac a);
  Lacaml.Io.pp_imat fmt a
