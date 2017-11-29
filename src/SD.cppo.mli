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
(** A type of transpose parameters ({!Common.normal} and
    {!Common.trans}).
    For complex matrices, {!Common.conjtr} is also offered, hence the name. *)

#include "SDCZ_common.mli"

module Vec :
sig
#include "SDCZ_vec.mli"
#include "SD_vec.mli"
end

module Mat :
sig
#include "SDCZ_mat.mli"
#include "SD_mat.mli"
end

#include "SDCZ_la.mli"
#include "SD_la.mli"
