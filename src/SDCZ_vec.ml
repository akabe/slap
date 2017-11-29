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

let wrap1
    (f : ?n:int -> ?ofsx:int -> ?incx:int -> I.vec -> 'a)
    x =
  let n, incx, x = V.__expose x in
  f ~n:(S.__expose n) ~ofsx:1 ~incx x

let wrap2
    (f : ?n:int ->
     ?ofsx:int -> ?incx:int -> I.vec ->
     ?ofsy:int -> ?incy:int -> I.vec -> 'a)
    x y =
  let n, incx, x = V.__expose x in
  let n', incy, y = V.__expose y in
  assert(n = n');
  f ~n:(S.__expose n) ~ofsx:1 ~incx x ~ofsy:1 ~incy y

let wrap2opt
    (f : ?n:int ->
     ?ofsy:int -> ?incy:int -> ?y:I.vec ->
     ?ofsx:int -> ?incx:int -> I.vec -> 'a)
    ?y x =
  let n, incx, x = V.__expose x in
  let incy, y = Vec.opt_vec_alloc prec n y in
  ignore (f ~n:(S.__expose n) ~ofsy:1 ~incy ~y ~ofsx:1 ~incx x);
  V.__unexpose n incy y

let wrap3
    (f : ?n:int ->
     ?ofsz:int -> ?incz:int -> I.vec ->
     ?ofsx:int -> ?incx:int -> I.vec ->
     ?ofsy:int -> ?incy:int -> I.vec -> 'a)
    z x y =
  let n, incx, x = V.__expose x in
  let n', incy, y = V.__expose y in
  let n'', incz, z = V.__expose z in
  assert(n = n' && n = n'');
  ignore (f ~n:(S.__expose n) ~ofsz:1 ~incz z ~ofsx:1 ~incx x ~ofsy:1 ~incy y);
  V.__unexpose n incz z

let wrap3opt
    (f : ?n:int ->
     ?ofsz:int -> ?incz:int -> ?z:I.vec ->
     ?ofsx:int -> ?incx:int -> I.vec ->
     ?ofsy:int -> ?incy:int -> I.vec -> 'a)
    ?z x y =
  let n, incx, x = V.__expose x in
  let n', incy, y = V.__expose y in
  assert(n = n');
  let incz, z = Vec.opt_vec_alloc prec n z in
  ignore (f ~n:(S.__expose n) ~ofsz:1 ~incz ~z ~ofsx:1 ~incx x ~ofsy:1 ~incy y);
  V.__unexpose n incz z

module type CNTVEC =
  sig
    type n
    val value : (n, 'cnt) vec
  end

module type DSCVEC =
  sig
    type n
    val value : (n, dsc) vec
  end

let cnt = Vec.cnt

(** {2 Creation of vectors} *)

let empty = Vec.create prec Size.zero

let create n = Vec.create prec n

let make n a = Vec.make prec n a

let make0 n = make n zero

let make1 n = make n one

let init n f = Vec.init prec n f

(** {2 Accessors} *)

let dim = Vec.dim

let get_dyn = Vec.get_dyn

let set_dyn = Vec.set_dyn

let unsafe_get = Vec.unsafe_get

let unsafe_set = Vec.unsafe_set

let replace_dyn = Vec.replace_dyn

(** {2 Basic operations} *)

let cons = Vec.cons

let hd = Vec.hd

let hd_dyn = Vec.hd_dyn

let last = Vec.last

let last_dyn = Vec.last_dyn

let tl = Vec.tl

let tl_dyn = Vec.tl_dyn

let inits = Vec.inits

let inits_dyn = Vec.inits_dyn

let copy ?y x = wrap2opt I.copy ?y x

let fill = Vec.fill

let append = Vec.append

let shared_rev = Vec.shared_rev

let rev = Vec.rev

(** {2 Type conversion} *)

let to_array = Vec.to_array

let of_array_dyn n array = Vec.of_array_dyn prec n array

module Of_array (X : sig val value : num_type array end) : CNTVEC =
  struct
    type n
    let value = Vec.unsafe_of_array prec
        (S.__unexpose (Array.length X.value)) X.value
  end

let of_array a =
  let module V = Of_array(struct let value = a end) in
  (module V : CNTVEC)

let unsafe_of_array n a = Vec.unsafe_of_array prec n a

let to_list = Vec.to_list

let of_list_dyn n list = Vec.of_list_dyn prec n list

module Of_list (X : sig val value : num_type list end) : CNTVEC =
  struct
    type n
    let value = Vec.unsafe_of_list prec
        (S.__unexpose (List.length X.value)) X.value
  end

let of_list l =
  let module V = Of_list(struct let value = l end) in
  (module V : CNTVEC)

let unsafe_of_list n l = Vec.unsafe_of_list prec n l

let to_bigarray = Vec.to_bigarray

let of_bigarray_dyn = Vec.of_bigarray_dyn

module Of_bigarray (X : sig
                          val share : bool
                          val value : (num_type, prec, fortran_layout) Array1.t
                        end) : CNTVEC =
  struct
    type n
    let value = Vec.unsafe_of_bigarray ~share:X.share
        (S.__unexpose (Array1.dim X.value)) X.value
  end

let of_bigarray ?(share=false) ba =
  let module V = Of_bigarray(struct let share = share
                                    let value = ba end) in
  (module V : CNTVEC)

let unsafe_of_bigarray = Vec.unsafe_of_bigarray


#if OCAML_MAJOR >= 4

let of_array_c arr = Vec.of_array_c prec arr

let of_list_c lst = Vec.of_list_c prec lst

let of_bigarray_c ?(share=false) ba = Vec.of_bigarray_c ~share ba

#endif


(** {2 Iterators} *)

let map f ?y x = Vec.map prec f ?y x

let mapi f ?y x = Vec.mapi prec f ?y x

let fold_left f init x = Vec.fold_left f init x

let fold_lefti f init x = Vec.fold_lefti f init x

let fold_right f x init = Vec.fold_right f x init

let fold_righti f x init = Vec.fold_righti f x init

let replace_all f x = Vec.replace_all f x

let replace_alli f x = Vec.replace_alli f x

let iter f x = Vec.iter f x

let iteri f x = Vec.iteri f x

(** {2 Iterators on two vectors} *)

let map2 f ?z x y = Vec.map2 prec f ?z x y

let mapi2 f ?z x y = Vec.mapi2 prec f ?z x y

let fold_left2 f init x y = Vec.fold_left2 f init x y

let fold_lefti2 f init x y = Vec.fold_lefti2 f init x y

let fold_right2 f x y init = Vec.fold_right2 f x y init

let fold_righti2 f x y init = Vec.fold_righti2 f x y init

let iter2 f x y = Vec.iter2 f x y

let iteri2 f x y = Vec.iteri2 f x y

(** {2 Iterators on three vectors} *)

let map3 f ?w x y z = Vec.map3 prec f ?w x y z

let mapi3 f ?w x y z = Vec.mapi3 prec f ?w x y z

let fold_left3 f init x y z = Vec.fold_left3 f init x y z

let fold_lefti3 f init x y z = Vec.fold_lefti3 f init x y z

let fold_right3 f x y z init = Vec.fold_right3 f x y z init

let fold_righti3 f x y z init = Vec.fold_righti3 f x y z init

let iter3 f x y z = Vec.iter3 f x y z

let iteri3 f x y z = Vec.iteri3 f x y z

(** {2 Scanning} *)

let for_all = Vec.for_all

let exists = Vec.exists

let for_all2 = Vec.for_all2

let exists2 = Vec.exists2

(** {2 Arithmetic operations} *)

let max x = wrap1 I.Vec.max x

let min x = wrap1 I.Vec.min x

let sum x = wrap1 I.Vec.sum x

let prod x = wrap1 I.Vec.prod x

let add_const c ?y x = wrap2opt (I.Vec.add_const c) ?y x

let sqr_nrm2 ?stable x = wrap1 (I.Vec.sqr_nrm2 ?stable) x

let ssqr ?c x = wrap1 (I.Vec.ssqr ?c) x

let sort ?cmp ?decr ?p x =
  let n, incx, x = V.__expose x in
  match p with
  | None ->
     I.Vec.sort ?cmp ?decr ~n:(S.__expose n) ~ofsx:1 ~incx x
  | Some p ->
    let n', incp, p = V.__expose p in
    assert(n = n');
    I.Vec.sort ?cmp ?decr ~n:(S.__expose n) ~ofsp:1 ~incp ~p ~ofsx:1 ~incx x

let neg ?y x = wrap2opt I.Vec.neg ?y x

let reci ?y x = wrap2opt I.Vec.reci ?y x

let add ?z x y = wrap3opt I.Vec.add ?z x y

let sub ?z x y = wrap3opt I.Vec.sub ?z x y

let mul ?z x y = wrap3opt I.Vec.mul ?z x y

let div ?z x y = wrap3opt I.Vec.div ?z x y

let zpxy z x y = wrap3 I.Vec.zpxy z x y

let zmxy z x y = wrap3 I.Vec.zmxy z x y

let ssqr_diff x y = wrap2 I.Vec.ssqr_diff x y

(** {2 Subvectors} *)

let subcntvec_dyn = Vec.subcntvec_dyn

let subdscvec_dyn = Vec.subdscvec_dyn

let subvec_dyn = Vec.subvec_dyn
