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

module F (I    : Slap_module_info.SDCZ)
         (SDCZ : Slap_lacaml.SDCZ with
            type num_type = I.num_type and
            type prec = I.prec and
            type trans3 = I.lacaml_trans) =
struct
  (* interface: slap_SDCZ_vec.ml *)

  module Vec = Slap_vec_impl

  include Slap_SDCZ_types_wrap.F(I)

  module type VEC =
    sig
      type n
      val value : (n, 'cnt) vec
    end

  (** {2 Creation of vectors} *)

  let create n = Vec.create I.kind n

  let make n a = Vec.make I.kind n a

  let zeros n = make n I.zero

  let ones n = make n I.one

  let make0 = zeros

  let init n ~f = Vec.init I.kind n ~f

  (** {2 Accessors} *)

  let dim = Vec.dim

  let get_dyn = Vec.get_dyn

  let set_dyn = Vec.set_dyn

  let unsafe_get = Vec.unsafe_get

  let unsafe_set = Vec.unsafe_set

  let replace_dyn = Vec.replace_dyn

  (** {2 Basic operations} *)

  let copy ?y (n, ofsx, incx, x) =
    let ofsy, incy, y = default_vec n y in
    let _ = SDCZ.copy ~n ~ofsy ~incy ~y ~ofsx ~incx x in
    (n, ofsy, incy, y)

  let fill = Vec.fill

  (** {2 Type conversion} *)

  let to_array = Vec.to_array

  let of_array_dyn n array = Vec.of_array_dyn I.kind n array

  module Of_array (X : sig val value : num_type array end) : VEC =
    struct
      type n
      let value = Vec.unsafe_of_array I.kind (Array.length X.value) X.value
    end

  let to_list = Vec.to_list

  let of_list_dyn n list = Vec.of_list_dyn I.kind n list

  module Of_list (X : sig val value : num_type list end) : VEC =
    struct
      type n
      let value = Vec.unsafe_of_list I.kind (List.length X.value) X.value
    end

  (** {2 Iterators} *)

  let map ?y ~f x = Vec.map I.kind ?y ~f x

  let mapi ?y ~f x = Vec.mapi I.kind ?y ~f x

  let fold_left = Vec.fold_left

  let fold_lefti = Vec.fold_lefti

  let fold_right = Vec.fold_right

  let fold_righti = Vec.fold_righti

  let replace_all = Vec.replace_all

  let replace_alli = Vec.replace_alli

  let iter = Vec.iter

  let iteri = Vec.iteri

  (** {2 Arithmetic operations} *)

  let max (n, ofsx, incx, x) = SDCZ.Vec.max ~n ~ofsx ~incx x

  let min (n, ofsx, incx, x) = SDCZ.Vec.min ~n ~ofsx ~incx x

  let sum (n, ofsx, incx, x) = SDCZ.Vec.sum ~n ~ofsx ~incx x

  let prod (n, ofsx, incx, x) = SDCZ.Vec.prod ~n ~ofsx ~incx x

  let sqr_nrm2 ?stable (n, ofsx, incx, x) =
    SDCZ.Vec.sqr_nrm2 ?stable ~n ~ofsx ~incx x

  let ssqr ?c (n, ofsx, incx, x) =
    SDCZ.Vec.ssqr ~n ?c ~ofsx ~incx x

  let sort ?cmp ?decr ?p (n, ofsx, incx, x) =
    match p with
    | None ->
       SDCZ.Vec.sort ?cmp ?decr ~n ~ofsx ~incx x
    | Some (n', ofsp, incp, p) ->
       assert(n = n');
       SDCZ.Vec.sort ?cmp ?decr ~n ~ofsp ~incp ~p ~ofsx ~incx x

  let neg ?y (n, ofsx, incx, x) =
    let ofsy, incy, y = default_vec n y in
    let _ = SDCZ.Vec.neg ~n ~ofsy ~incy ~y ~ofsx ~incx x in
    (n, ofsy, incy, y)

  let add ?z (n, ofsx, incx, x) (n', ofsy, incy, y) =
    assert(n = n');
    let ofsz, incz, z = default_vec n z in
    let _ = SDCZ.Vec.add ~n ~ofsz ~incz ~z ~ofsx ~incx x ~ofsy ~incy y in
    (n, ofsz, incz, z)

  let sub ?z (n, ofsx, incx, x) (n', ofsy, incy, y) =
    assert(n = n');
    let ofsz, incz, z = default_vec n z in
    let _ = SDCZ.Vec.sub ~n ~ofsz ~incz ~z ~ofsx ~incx x ~ofsy ~incy y in
    (n, ofsz, incz, z)

  let mul ?z (n, ofsx, incx, x) (n', ofsy, incy, y) =
    assert(n = n');
    let ofsz, incz, z = default_vec n z in
    let _ = SDCZ.Vec.mul ~n ~ofsz ~incz ~z ~ofsx ~incx x ~ofsy ~incy y in
    (n, ofsz, incz, z)

  let div ?z (n, ofsx, incx, x) (n', ofsy, incy, y) =
    assert(n = n');
    let ofsz, incz, z = default_vec n z in
    let _ = SDCZ.Vec.div ~n ~ofsz ~incz ~z ~ofsx ~incx x ~ofsy ~incy y in
    (n, ofsz, incz, z)

  let ssqr_diff (n, ofsx, incx, x) (n', ofsy, incy, y) =
    assert(n = n');
    SDCZ.Vec.ssqr_diff ~n ~ofsx ~incx x ~ofsy ~incy y
end
