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

(** {2 Creation of vectors} *)

val random : ?rnd_state:Random.State.t ->
             ?re_from:float -> ?re_range:float ->
             ?im_from:float -> ?im_range:float ->
             'm Slap_size.t -> 'n Slap_size.t -> ('m, 'n, 'cnt) mat
(** [random ?rnd_state ?from ?range m n] creates a [m]-by-[n] matrix randomly
    initialized with the uniform distribution between [re_from]/[im_from] and
    [re_from+re_range]/[im_from+im_range] for real and imaginary parts,
    respectively.

    @param rnd_state default = [Random.get_state ()].
    @param re_from   default = [-1.0].
    @param re_range  default = [2.0].
    @param im_from   default = [-1.0].
    @param im_range  default = [2.0].

    @since 0.2.0
 *)
