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

module F (I    : Slap_module_info.SD)
         (SDCZ : Slap_lacaml.SDCZ_SD with type prec = I.prec)
         (SD   : Slap_lacaml.SD      with type prec = I.prec) =
struct
  (* interface: slap_SD.ml *)

  include Slap_SD_la_wrap.F(I)(SDCZ)(SD)

  module Vec = Slap_SD_vec_wrap.F(I)(SDCZ)(SD)

  module Mat = Slap_SD_mat_wrap.F(I)(SDCZ)(SD)
end