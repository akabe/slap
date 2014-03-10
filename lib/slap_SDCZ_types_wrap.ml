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

module F (I : Slap_module_info.SDCZ) =
struct
  (* interface: slap_SDCZ_types.ml *)

  module Common = Slap_common_impl

  type num_type = I.num_type
  type prec = I.prec
  type real_prec = I.real_prec
  type (+'n, +'cnt_or_dsc) vec = ('n, num_type, prec, 'cnt_or_dsc) Common.vec
  type (+'n, +'cnt_or_dsc) real_vec =
      ('n, float, real_prec, 'cnt_or_dsc) Common.vec
  type (+'m, +'n, +'cnt_or_dsc) mat =
      ('m, 'n, num_type, prec, 'cnt_or_dsc) Common.mat
  type +'a trans = ('a, I.trans_tag) Common.trans

  (* internal functions *)

  let module_path = "Slap." ^ I.module_name

  let invalid_arg msg = invalid_arg (module_path ^ "." ^ msg)

  let default_vec n x = Slap_vec_impl.default_vec I.kind n x

  let default_mat m n a = Slap_mat_impl.default_mat I.kind m n a

  let get_transposed_dim trans m n =
    match trans with
    | `N -> (m, n)
    | `T | `C -> (n, m)

  let lacaml_trans3_of_trans = I.lacaml_trans_of_trans

  let lacaml_trans2_of_trans = function
    | `N | `T as trans -> trans
    | `C -> assert(false)
end
