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

type cnt
(** The tag for contiguous vectors and matrices. *)

type dsc
(** The tag for discrete vectors and matrices. *)

(** Sizes (dimensions of vectors and matrices). *)
module Size :
sig
  #include "slap_size.mli"
end

(** Sized vectors. *)
module Vec :
sig
  #include "slap_vec.mli"
end

(** Sized matrices. *)
module Mat :
sig
  #include "slap_mat.mli"
end

(** {2 Pretty printing} *)

(** Pretty-printers of vector and matrices. *)
module Io :
sig
  #include "slap_io.mli"
end

(** {2 Precision dependent modules} *)

(** Types, flags and functions common to all precision dependent modules. *)
module Common :
sig
  #include "slap_common.mli"
end

(** 64-bit real BLAS and LAPACK functions. *)
module D :
sig
#define SLAP_SDCZ_BITS 64
#include "slap_SD.mli"
#undef SLAP_SDCZ_BITS
end

(** 32-bit real BLAS and LAPACK functions. *)
module S :
sig
#define SLAP_SDCZ_BITS 32
#include "slap_SD.mli"
#undef SLAP_SDCZ_BITS
end

(** 64-bit complex BLAS and LAPACK functions. *)
module Z :
sig
#define SLAP_SDCZ_BITS 64
#include "slap_CZ.mli"
#undef SLAP_SDCZ_BITS
end

(** 32-bit complex BLAS and LAPACK functions. *)
module C :
sig
#define SLAP_SDCZ_BITS 32
#include "slap_CZ.mli"
#undef SLAP_SDCZ_BITS
end
