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

module F (I    : Slap_module_info.CZ)
         (SDCZ : Slap_lacaml.SDCZ_CZ with
            type prec = I.prec and
            type rvec = I.lacaml_rvec)
         (CZ   : Slap_lacaml.CZ with
            type prec = I.prec and
            type rvec = I.lacaml_rvec) =
struct
  (* interface: slap_CZ.ml *)

  include Slap_CZ_la_wrap.F(I)(SDCZ)(CZ)

  module Vec = Slap_CZ_vec_wrap.F(I)(SDCZ)(CZ)

  module Mat = Slap_CZ_mat_wrap.F(I)(SDCZ)(CZ)
end
