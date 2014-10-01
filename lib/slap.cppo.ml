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

type cnt
type dsc

let (|>) x f = f x

module Size =
  struct
    #include "slap_size.ml"
  end

module Vec =
  struct
    #include "slap_vec.ml"
  end

module Mat =
  struct
    #include "slap_mat.ml"
  end

(** {2 Pretty printing} *)

module Io =
  struct
    #include "slap_io.ml"
  end

(** {2 Precision dependent modules} *)

module Common =
  struct
#include "slap_common.ml"
  end

module D =
  struct
#define SLAP_SDCZ_BITS 64
#include "slap_SD.ml"
#undef SLAP_SDCZ_BITS
  end

module S =
  struct
#define SLAP_SDCZ_BITS 32
#include "slap_SD.ml"
#undef SLAP_SDCZ_BITS
  end

module Z =
  struct
#define SLAP_SDCZ_BITS 64
#include "slap_CZ.ml"
#undef SLAP_SDCZ_BITS
  end

module C =
  struct
#define SLAP_SDCZ_BITS 32
#include "slap_CZ.ml"
#undef SLAP_SDCZ_BITS
  end
