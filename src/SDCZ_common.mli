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

type (+'n, +'cnt_or_dsc) vec =
  ('n, num_type, prec, 'cnt_or_dsc) Vec.t
(** Vectors. *)

type (+'m, +'n, +'cnt_or_dsc) mat =
  ('m, 'n, num_type, prec, 'cnt_or_dsc) Mat.t
(** Matrices. *)

type rprec = CONCAT(CONCAT(float, XBITS), _elt)

type (+'n, +'cnt_or_dsc) rvec = ('n, float, rprec, 'cnt_or_dsc) Vec.t
(** Real vectors. (In {!Slap.S} and {!Slap.D}, [rvec] is equal to [vec].) *)

val prec : (num_type, prec) kind

val rprec : (float, rprec) kind
