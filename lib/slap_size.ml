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

(** The signature of {!Slap.Size}. *)

module type S =
sig
  (* implementation: slap_size_impl.ml *)

  module Common : Slap_common.S (** = {!Slap.Common} *)

  open Common

  (** {2 Constants and arithmetric operations} *)

  val zero : z size

  val one : z s size

  val succ : 'n size -> 'n s size

  val add : 'm size -> 'n size -> ('m, 'n) add size

  val mul : 'm size -> 'n size -> ('m, 'n) mul size

  val min : 'm size -> 'n size -> ('m, 'n) min size

  val max : 'm size -> 'n size -> ('m, 'n) max size

  (** {2 Iterators}

      The following functions are iterators over [[1; 2; ...; n]] where [n] is
      a size.
  *)

  val fold_left : f:('a -> int -> 'a) ->
    init:'a ->
    'n size -> 'a
  (** [fold_left ~f ~init n] is
      [f (... (f (f init 1) 2) ...) (to_int n)].
  *)

  val fold_right : f:(int -> 'a -> 'a) ->
    'n size ->
    init:'a -> 'a
  (** [fold_right ~f n ~init] is
      [f 1 (f 2 (... (f (to_int n) init) ...))].
  *)

  val iter : f:(int -> unit) -> 'n size -> unit
  (** [iter ~f n] is [f 1; f 2; ...; f (to_int n)].
  *)

  val riter : f:(int -> unit) -> 'n size -> unit
  (** [riter ~f n] is [f (to_int n); ...; f 2; f 1].
  *)

  (** {2 Conversion between sizes and integers} *)

  val to_int : 'n size -> int
  (** Return the integer correponding to the given size. *)

  module type SIZE =
  sig
    type n
    val value : n size
  end

  module Of_int_dyn : functor (N : sig val value : int end) -> SIZE
end
