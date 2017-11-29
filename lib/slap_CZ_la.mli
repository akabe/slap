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

(** {2 BLAS interface} *)

(** {3 Level 1} *)

val dotu : ('n, 'x_cd) vec -> ('n, 'y_cd) vec -> num_type
(** [dotc x y] computes [x^T y].
    @return an inner product of given two vectors.
    @since 2.0.0 *)

val dotc : ('n, 'x_cd) vec -> ('n, 'y_cd) vec -> num_type
(** [dotc x y] computes [x^H y].
    @return an inner product of a conjugated vector with another vector.
    @since 2.0.0 *)
