(* Sized Linear Algebra Package (SLAP)

   Copyright (C) 2013- Akinori ABE <aabe.65535@gmail.com>

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
open Misc
open Common

type prec = CONCAT(CONCAT(float, XBITS), _elt)
type num_type = float
type (+'indim, +'outdim, +'tag) trans3 =
  ('indim, 'outdim, 'tag) Common.trans2
let prec = CONCAT(float, XBITS)
let zero = 0.0
let one = 1.0
let lacaml_trans3 = Common.lacaml_trans2
let int_of_num = int_of_float

#include "SDCZ_common.ml"

module Vec =
struct
#include "SDCZ_vec.ml"
#include "SD_vec.ml"
end

module Mat =
struct
#include "SDCZ_mat.ml"
#include "SD_mat.ml"
end

#include "SDCZ_la.ml"
#include "SD_la.ml"
