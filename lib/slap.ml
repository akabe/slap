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

(** Types, flags and functions commonly used in SLAP. *)
module Common = Slap_common

(** Sizes (dimensions of vectors and matrices). *)
module Size = Slap_size

(** Sized vectors. *)
module Vec = Slap_vec

(** Sized matrices. *)
module Mat = Slap_mat

(** Pretty printers. *)
module Io = Slap_io

type cnt = Slap_common.cnt
(** For compatilibity with SLAP version 0 *)

type dsc = Slap_common.dsc
(** For compatilibity with SLAP version 0 *)

(** {2 Precision dependent modules} *)

(** 64-bit real BLAS and LAPACK functions. *)
module D = Slap_D

(** 32-bit real BLAS and LAPACK functions. *)
module S = Slap_S

(** 64-bit complex BLAS and LAPACK functions. *)
module Z = Slap_Z

(** 32-bit complex BLAS and LAPACK functions. *)
module C = Slap_C
