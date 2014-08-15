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

type +'n t = int

(** {2 Constants} *)

type z

type 'n s

let zero = 0

let one = 1

let two = 2

let three = 3

let four = 4

let five = 5

type ten = z s s s s s s s s s s

let ten = 10

(** {2 Arithmetric operations} *)

let succ = Pervasives.succ

type ('m, 'n) add

let add = ( + )

type ('m, 'n) sub

let sub_dyn m n =
  if m >= n then m - n else invalid_arg "Slap.Size.sub_dyn: negative integer"

type ('m, 'n) mul

let mul = ( * )

type ('m, 'n) div

let div_dyn m n =
  if n <> 0 then m / n else invalid_arg "Slap.Size.div_dyn: zero division"

type ('m, 'n) min

let min = Pervasives.min

type ('m, 'n) max

let max = Pervasives.max

(** {2 Conversion between sizes and integers} *)

module type SIZE =
  sig
    type n
    val value : n t
  end

let to_int n = n

let unsafe_of_int (n : int) =
  let module N =
    struct
      type n
      let value = n
    end in
  (module N : SIZE)

let of_int_dyn n =
  if n < 0 then invalid_arg "Slap.Size.of_int_dyn";
  unsafe_of_int n

module Of_int_dyn (N : sig val value : int end) : SIZE =
struct
  type n
  let value =
    if N.value < 0 then invalid_arg "Slap.Size.Of_int_dyn";
    N.value
end

(** {2 Iterators on integers} *)

let fold_lefti f init n =
  let rec loop i e =
    if i > n then e else loop (i + 1) (f e i)
  in
  loop 1 init

let fold_righti f n init =
  let rec loop i e =
    if i = 0 then e else loop (i - 1) (f i e)
  in
  loop n init

let iteri f n = for i = 1 to n do f i done

let riteri f n = for i = n downto 1 do f i done

(** {2 Iterators on sizes} *)

let fold_left f = fold_lefti (fun acc i -> f acc (unsafe_of_int i))

let fold_right f = fold_righti (fun i -> f (unsafe_of_int i))

let iter f = iteri (fun i -> f (unsafe_of_int i))

let riter f = riteri (fun i -> f (unsafe_of_int i))
