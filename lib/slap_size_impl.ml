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

(* interface: slap_common.ml *)

module Common = Slap_common_impl

open Common

(** {2 Constants and arithmetric operations} *)

let zero = 0

let one = 1

let succ = Pervasives.succ

let add = ( + )

let mul = ( * )

let min = Pervasives.min

let max = Pervasives.max

(** {2 Iterators} *)

let fold_left ~f ~init n =
  let rec loop i e =
    if i > n then e else loop (i + 1) (f e i)
  in
  loop 1 init

let fold_right ~f n ~init =
  let rec loop i e =
    if i = 0 then e else loop (i - 1) (f i e)
  in
  loop n init

let iter ~f n = for i = 1 to n do f i done

let riter ~f n = for i = n downto 1 do f i done

(** {2 Conversion between a size and an integer} *)

let to_int n = n

module type SIZE =
sig
  type n
  val value : n size
end

module Of_int_dyn =
  functor (N : sig val value : int end) ->
struct
  type n
  let value =
    if N.value < 0 then invalid_arg "Slap.Size.Of_int_dyn";
    N.value
end
