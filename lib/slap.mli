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

(** SLAP (Sized Linear Algebra Package) *)

(** Common types, flags and functions. *)
module Common : Slap_common.S

(** Operations on sizes. *)
module Size : Slap_size.S with module Common = Common

(** Operations on vectors. *)
module Vec : Slap_vec.S with module Common = Common

(** Operations on matrices. *)
module Mat : Slap_mat.S with module Common = Common

(** Utility functions. *)
module Utils : Slap_utils.S with module Common = Common

(** {2 Pretty printing} *)

module Io : Slap_io.S with module Common = Common

(** {2 Precision-dependent modules}

  [S], [D], [C] and [Z] correspond to types of elements of vectors and matrices:
  - [S] : Single-precision (32-bit) float
  - [D] : Double-precision (64-bit) float
  - [C] : Single-precision (32-bit) complex
  - [Z] : Double-precision (64-bit) complex
 *)

module S : Slap_SD.S with
         module Common = Common and
         type num_type = float and
         type prec = Bigarray.float32_elt and
         type real_prec = Bigarray.float32_elt and
         type 'a trans = 'a Common.trans2

module D : Slap_SD.S with
         module Common = Common and
         type num_type = float and
         type prec = Bigarray.float64_elt and
         type real_prec = Bigarray.float64_elt and
         type 'a trans = 'a Common.trans2

module C : Slap_CZ.S with
         module Common = Common and
         type num_type = Complex.t and
         type prec = Bigarray.complex32_elt and
         type real_prec = Bigarray.float32_elt and
         type 'a trans = 'a Common.trans3

module Z : Slap_CZ.S with
         module Common = Common and
         type num_type = Complex.t and
         type prec = Bigarray.complex64_elt and
         type real_prec = Bigarray.float64_elt and
         type 'a trans = 'a Common.trans3
