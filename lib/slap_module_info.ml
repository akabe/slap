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

module Common = Slap_common_impl

open Bigarray

module type SDCZ =
sig
  val module_name : string

  type num_type
  val zero : num_type
  val one : num_type

  type prec
  val kind : (num_type, prec) kind

  type real_prec
  val real_kind : (float, real_prec) kind

  type lacaml_vec  = (num_type, prec, fortran_layout) Array1.t
  type lacaml_rvec = (float, real_prec, fortran_layout) Array1.t
  type lacaml_mat  = (num_type, prec, fortran_layout) Array2.t
  type lacaml_trans

  type trans_tag
  val lacaml_trans_of_trans : ('a, trans_tag) Common.trans -> lacaml_trans
end

module type SD = SDCZ with
    type num_type = float and
    type lacaml_trans = [ `N | `T ] and
    type trans_tag = Common.trans2_tag

module type CZ = SDCZ with
    type num_type = Complex.t and
    type lacaml_trans = [ `N | `T | `C ] and
    type trans_tag = Common.trans3_tag

module MakeSD (X :
  sig
    type prec
    val kind : (float, prec) kind
    val module_name : string
  end) =
struct
  let module_name = X.module_name

  type num_type = float
  let zero = 0.0
  let one = 1.0

  type prec = X.prec
  let kind = X.kind

  type real_prec = X.prec
  let real_kind = X.kind

  type lacaml_vec   = (num_type, prec, fortran_layout) Array1.t
  type lacaml_rvec  = lacaml_vec
  type lacaml_mat   = (num_type, prec, fortran_layout) Array2.t
  type lacaml_trans = [ `N | `T ]

  type trans_tag = Common.trans2_tag
  let lacaml_trans_of_trans t =
    assert(t = `N || t = `T);
    match t with
    | `N -> `N
    | _  -> `T
end

module MakeCZ (X :
  sig
    type prec
    type real_prec
    val kind  : (Complex.t, prec) kind
    val real_kind : (float, real_prec) kind
    val module_name : string
  end) =
struct
  let module_name = X.module_name

  type num_type = Complex.t
  let zero = Complex.zero
  let one = Complex.one

  type prec = X.prec
  let kind = X.kind

  type real_prec = X.real_prec
  let real_kind = X.real_kind

  type lacaml_vec   = (num_type, prec, fortran_layout) Array1.t
  type lacaml_rvec  = (float, real_prec, fortran_layout) Array1.t
  type lacaml_mat   = (num_type, prec, fortran_layout) Array2.t
  type lacaml_trans = [ `N | `T | `C ]

  type trans_tag = Common.trans3_tag
  let lacaml_trans_of_trans t = t
end
