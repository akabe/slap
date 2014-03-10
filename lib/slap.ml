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

module Common = Slap_common_impl

module Size = Slap_size_impl

module Vec = Slap_vec_impl

module Mat = Slap_mat_impl

module Utils = Slap_utils_impl

module Io = Slap_io_impl

module S =
struct
  module I = Slap_module_info.MakeSD
               (struct
                 type prec       = Bigarray.float32_elt
                 let kind        = Bigarray.float32
                 let module_name = "S"
               end)

  include Slap_SD_wrap.F(I)(Lacaml.S)(Lacaml.S)
end

module D =
struct
  module I = Slap_module_info.MakeSD
               (struct
                 type prec       = Bigarray.float64_elt
                 let kind        = Bigarray.float64
                 let module_name = "D"
               end)

  include Slap_SD_wrap.F(I)(Lacaml.D)(Lacaml.D)
end

module C =
struct
  module I = Slap_module_info.MakeCZ
               (struct
                 type prec       = Bigarray.complex32_elt
                 type real_prec  = Bigarray.float32_elt
                 let kind        = Bigarray.complex32
                 let real_kind   = Bigarray.float32
                 let module_name = "C"
               end)

  include Slap_CZ_wrap.F(I)(Lacaml.C)(Lacaml.C)
end

module Z =
struct
  module I = Slap_module_info.MakeCZ
               (struct
                 type prec       = Bigarray.complex64_elt
                 type real_prec  = Bigarray.float64_elt
                 let kind        = Bigarray.complex64
                 let real_kind   = Bigarray.float64
                 let module_name = "Z"
               end)

  include Slap_CZ_wrap.F(I)(Lacaml.Z)(Lacaml.Z)
end
