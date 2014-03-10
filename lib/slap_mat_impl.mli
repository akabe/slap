(* Sized Linear Algebra Package (SLAP)

  Copyright (C) 2013- Akinori ABE <abe@kb.ecei.tohoku.ac.jp>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

include Slap_mat.S with module Common = Slap_common_impl

open Common

(** {2 Internal functions} *)

val check_cnt : int -> int -> int -> int ->
                ('num, 'prec, Bigarray.fortran_layout) Bigarray.Array2.t -> bool

val default_mat : ('num, 'prec) Bigarray.kind ->
                  'm size ->
                  'n size ->
                  ('m, 'n, 'num, 'prec, 'cd) mat option ->
                  int * int * ('num, 'prec, Bigarray.fortran_layout)
                                Bigarray.Array2.t

val unsafe_of_array : ('num, 'prec) Bigarray.kind ->
                      'm size ->
                      'n size ->
                      'num array array ->
                      ('m, 'n, 'num, 'prec, 'cnt) mat
(** Like {!Slap.Mat.of_array_dyn}, but size checking is not performed. *)

val unsafe_of_list : ('num, 'prec) Bigarray.kind ->
                     'm size ->
                     'n size ->
                     'num list list ->
                     ('m, 'n, 'num, 'prec, 'cnt) mat
(** Like {!Slap.Mat.of_list_dyn}, but size checking is not performed. *)
