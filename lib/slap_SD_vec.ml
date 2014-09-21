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

(** {2 Creation of vectors} *)

let random ?rnd_state ?from ?range n =
  let x = I.Vec.random ?rnd_state ?from ?range n in
  (n, 1, 1, x)

(** {2 Arithmetic operations} *)

let sqr ?y (n, ofsx, incx, x) =
  let ofsy, incy, y = PVec.opt_vec_alloc prec n y in
  let _ = I.Vec.sqr ~n ~ofsy ~incy ~y ~ofsx ~incx x in
  (n, ofsy, incy, y)

let sqrt ?y (n, ofsx, incx, x) =
  let ofsy, incy, y = PVec.opt_vec_alloc prec n y in
  let _ = I.Vec.sqrt ~n ~ofsy ~incy ~y ~ofsx ~incx x in
  (n, ofsy, incy, y)

let exp ?y (n, ofsx, incx, x) =
  let ofsy, incy, y = PVec.opt_vec_alloc prec n y in
  let _ = I.Vec.exp ~n ~ofsy ~incy ~y ~ofsx ~incx x in
  (n, ofsy, incy, y)

let log ?y (n, ofsx, incx, x) =
  let ofsy, incy, y = PVec.opt_vec_alloc prec n y in
  let _ = I.Vec.log ~n ~ofsy ~incy ~y ~ofsx ~incx x in
  (n, ofsy, incy, y)

let sin ?y (n, ofsx, incx, x) =
  let ofsy, incy, y = PVec.opt_vec_alloc prec n y in
  let _ = I.Vec.sin ~n ~ofsy ~incy ~y ~ofsx ~incx x in
  (n, ofsy, incy, y)

let cos ?y (n, ofsx, incx, x) =
  let ofsy, incy, y = PVec.opt_vec_alloc prec n y in
  let _ = I.Vec.cos ~n ~ofsy ~incy ~y ~ofsx ~incx x in
  (n, ofsy, incy, y)
