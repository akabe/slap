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

open Slap_misc

module S = Slap_size

type ('n, 'a) t = 'a array

module type ARRAY =
sig
  type n   (** the length of an array *)
  type elt (** the type of elements *)
  val value : (n, elt) t
end
(** {2 Creation of sized arrays} *)

let make n x = Array.make (S.__expose n) x

let init n f = Array.init (S.__expose n) f

let make_matrix m n x =
  Array.make_matrix (S.__expose m) (S.__expose n) x

let init_matrix m n f = init m (fun i -> init n (f i))

(** {2 Base operations} *)

let length a = S.__unexpose (Array.length a)

let get_dyn = Array.get

let set_dyn = Array.set

let unsafe_get = Array.unsafe_get

let unsafe_set = Array.unsafe_set

let replace_dyn a i f =
  if i < 0 || i >= Array.length a
  then invalid_arg "index out of bounds";
  unsafe_set a i (f (unsafe_get a i))

let copy = Array.copy

let append = Array.append

let concat (type a) (l : ('n, a) t list) =
  (module struct
     type n
     type elt = a
     let value = Array.concat l
   end : ARRAY with type elt = a)

module Concat (X : sig
                 type n
                 type elt
                 val value : (n, elt) t list
               end) :
  ARRAY with type elt = X.elt =
struct
  type n
  type elt = X.elt
  let value = Array.concat X.value
end

(** {2 Iterators} *)

let mapi = Array.mapi

let map = Array.map

let fold_lefti f init a =
  let n = Array.length a in
  let rec loop i acc =
    if i >= n then acc else loop (i + 1) (f i acc (Array.unsafe_get a i))
  in
  loop 0 init

let fold_left = Array.fold_left

let fold_righti f a init =
  let rec loop i acc =
    if i < 0 then acc else loop (i - 1) (f i (Array.unsafe_get a i) acc)
  in
  loop (Array.length a - 1) init

let fold_right = Array.fold_right

let iter = Array.iter

let iteri = Array.iteri

let replace_all a f = iteri (fun i x -> unsafe_set a i (f x)) a

let replace_alli a f = iteri (fun i x -> unsafe_set a i (f i x)) a

(** {2 Iterators on two sized arrays} *)

let mapi2 f a1 a2 =
  assert(Array.length a1 = Array.length a2);
  mapi (fun i x1 -> f i x1 (unsafe_get a2 i)) a1

let map2 f = mapi2 (fun _ -> f)

let fold_lefti2 f init a1 a2 =
  assert(Array.length a1 = Array.length a2);
  fold_lefti (fun i acc x1 -> f i acc x1 (unsafe_get a2 i)) init a1

let fold_left2 f = fold_lefti2 (fun _ -> f)

let fold_righti2 f a1 a2 init =
  assert(Array.length a1 = Array.length a2);
  fold_righti (fun i x1 -> f i x1 (unsafe_get a2 i)) a1 init

let fold_right2 f = fold_righti2 (fun _ -> f)

let iteri2 f a1 a2 =
  assert(Array.length a1 = Array.length a2);
  iteri (fun i x1 -> f i x1 (unsafe_get a2 i)) a1

let iter2 f = iteri2 (fun _ -> f)

(** {2 Iterators on three sized arrays} *)

let mapi3 f a1 a2 a3 =
  assert(Array.length a1 = Array.length a2 &&
         Array.length a2 = Array.length a3);
  mapi2 (fun i x1 x2 -> f i x1 x2 (unsafe_get a3 i)) a1 a2

let map3 f = mapi3 (fun _ -> f)

let fold_lefti3 f init a1 a2 a3 =
  assert(Array.length a1 = Array.length a2 &&
         Array.length a2 = Array.length a3);
  fold_lefti2 (fun i acc x1 x2 -> f i acc x1 x2 (unsafe_get a3 i)) init a1 a2

let fold_left3 f = fold_lefti3 (fun _ -> f)

let fold_righti3 f a1 a2 a3 init =
  assert(Array.length a1 = Array.length a2 &&
         Array.length a2 = Array.length a3);
  fold_righti2 (fun i x1 x2 -> f i x1 x2 (unsafe_get a3 i)) a1 a2 init

let fold_right3 f = fold_righti3 (fun _ -> f)

let iteri3 f a1 a2 a3 =
  assert(Array.length a1 = Array.length a2 &&
         Array.length a2 = Array.length a3);
  iteri2 (fun i x1 x2 -> f i x1 x2 (unsafe_get a3 i)) a1 a2

let iter3 f = iteri3 (fun _ -> f)

(** {2 Scanning} *)

let for_all f a =
  S.for_alli
    (fun i -> f (unsafe_get a (i - 1)))
    (S.__unexpose (Array.length a))

let exists f a =
  S.existsi
    (fun i -> f (unsafe_get a (i - 1)))
    (S.__unexpose (Array.length a))

let for_all2 f a1 a2 =
  assert(Array.length a1 = Array.length a2);
  S.for_alli
    (fun i ->
       let j = i - 1 in
       f (unsafe_get a1 j) (unsafe_get a2 j))
    (S.__unexpose (Array.length a1))

let exists2 f a1 a2 =
  assert(Array.length a1 = Array.length a2);
  S.existsi
    (fun i ->
       let j = i - 1 in
       f (unsafe_get a1 j) (unsafe_get a2 j))
    (S.__unexpose (Array.length a1))

(** {2 Conversion} *)

let to_array = identity

let of_array_dyn n a =
  if S.__expose n <> Array.length a
  then invalid_arg "size unmatched";
  a

let of_array (type a) (a : a array) =
  (module struct
     type n
     type elt = a
     let value = a
   end : ARRAY with type elt = a)

module Of_array (X : sig
                   type elt
                   val value : elt array
                 end) :
  ARRAY with type elt = X.elt =
struct
  type n
  type elt = X.elt
  let value = X.value
end

let to_list = Array.to_list

let of_list_dyn n l =
  if S.__expose n <> List.length l
  then invalid_arg "size unmatched";
  Array.of_list l

let of_list (type a) (l : a list) =
  (module struct
     type n
     type elt = a
     let value = Array.of_list l
   end : ARRAY with type elt = a)

module Of_list (X : sig
                  type elt
                  val value : elt list
                end) :
  ARRAY with type elt = X.elt =
struct
  type n
  type elt = X.elt
  let value = Array.of_list X.value
end

(** {2 Sorting} *)

let sort = Array.sort

let stable_sort = Array.stable_sort

let fast_sort = Array.fast_sort
