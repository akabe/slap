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

(** Types used in [Slap.[SDCZ]]. *)

module type S =
sig
  (* implementation: slap_SDCZ_types_wrap.ml *)

  module Common : Slap_common.S (** = {!Slap.Common}. *)

  type num_type
  (** The OCaml type of elements of vectors and matrices:
   - [type num_type = float] in {!Slap.S} and {!Slap.D}.
   - [type num_type = Complex.t] in {!Slap.C} and {!Slap.Z}.
   *)

  type prec
  (** The reprensentation kind of vectors and matrices:
   - [type prec = Bigarray.float32_elt] in {!Slap.S}.
   - [type prec = Bigarray.float64_elt] in {!Slap.D}.
   - [type prec = Bigarray.complex32_elt] in {!Slap.C}.
   - [type prec = Bigarray.complex64_elt] in {!Slap.Z}.
   *)

  type real_prec
  (** The reprensentation kind of real vectors and matrices:
   - [type real_prec = prec] in {!Slap.S} and {!Slap.D}.
   - [type real_prec = Bigarray.float32_elt] in {!Slap.C}.
   - [type real_prec = Bigarray.float64_elt] in {!Slap.Z}.
   *)

  type (+'n, +'cnt_or_dsc) vec = ('n, num_type, prec, 'cnt_or_dsc) Common.vec
  (** The type of ['n]-dimensional real/complex vectors. *)

  type (+'n, +'cnt_or_dsc) real_vec =
      ('n, float, real_prec, 'cnt_or_dsc) Common.vec
  (** The type of ['n]-dimensional real vectors. *)

  type (+'m, +'n, +'cnt_or_dsc) mat =
      ('m, 'n, num_type, prec, 'cnt_or_dsc) Common.mat
  (** The type of ['m]-by-['n] real/complex matrices. *)

  type +'a trans
  (** The type of transpose flags:
   - [type 'a trans = 'a Slap.Common.trans2] in {!Slap.S} and {!Slap.D}.
   - [type 'a trans = 'a Slap.Common.trans3] in {!Slap.C} and {!Slap.Z}.
   *)
end
