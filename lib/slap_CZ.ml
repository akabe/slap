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

type prec = CONCAT(CONCAT(complex, SLAP_SDCZ_BITS), _elt)
type num_type = Complex.t
type 'a trans3 = 'a Common.trans3
let prec = CONCAT(complex, SLAP_SDCZ_BITS)
let zero = Complex.zero
let one = Complex.one
let lacaml_trans3 t = t

#if SLAP_SDCZ_BITS = 32
module I = Lacaml.C
let module_name = "Slap.C"
#else
module I = Lacaml.Z
let module_name = "Slap.Z"
#endif

#include "slap_SDCZ_common.ml"

module Vec =
  struct
#include "slap_SDCZ_vec.ml"
#include "slap_CZ_vec.ml"
  end

module Mat =
  struct
#include "slap_SDCZ_mat.ml"
#include "slap_CZ_mat.ml"
  end

#include "slap_SDCZ_la.ml"
#include "slap_CZ_la.ml"
