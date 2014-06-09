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

val random : ?rnd_state:Random.State.t ->
             ?from:float -> ?range:float -> 'n Size.t -> ('n, 'cnt) vec

(** {2 Arithmetic operations} *)

val sqr : ?y:('n, 'y_cd) vec -> ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [sqr ?y x] computes the square of elements of the vector [x].
    @return the vector [y], which is overwritten.
 *)

val sqrt : ?y:('n, 'y_cd) vec -> ('n, 'x_cd) vec -> ('n, 'y_cd) vec
 (** [sqr ?y x] computes the square root of elements of the vector [x].
     @return the vector [y], which is overwritten.
  *)
