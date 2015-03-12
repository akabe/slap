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

(** {!Slap.Array} provides operations on sized arrays.
    @since 1.0.0 *)

type ('n, 'a) t = private 'a array
(** [('n, 'a) t] is the typed of sized arrays whose elements have type ['a] and
    length is ['n]. *)

module type ARRAY =
sig
  type n   (** the length of an array *)
  type elt (** the type of elements *)
  val value : (n, elt) t
end

(** {2 Creation of sized arrays} *)

val make : 'n Slap_size.t -> 'a -> ('n, 'a) t
(** [make n x] returns an array of length [n]. All elements are initialized with
    [x]. *)

val init : 'n Slap_size.t -> (int -> 'a) -> ('n, 'a) t
(** [init n f] returns an array of length [n]. The [i]-th element is initialized
    by [f i]. *)

val make_matrix :
  'm Slap_size.t -> 'n Slap_size.t -> 'a -> ('m, ('n, 'a) t) t
(** [make_matrix m n x] returns a two-dimensional array (an array of arrays)
    with first dimension [m] and second dimension [n]. All elements are
    initialized with [x]. *)

val init_matrix :
  'm Slap_size.t -> 'n Slap_size.t -> (int -> int -> 'a) -> ('m, ('n, 'a) t) t
(** [init_matrix m n f] returns a two-dimensional array (an array of arrays)
    with first dimension [m] and second dimension [n]. The [(i,j)] element is
    initialized by [f i j]. *)

(** {2 Base operations} *)

val length : ('n, 'a) t -> 'n Slap_size.t
(** Return the length of the given array. *)

val get_dyn : ('n, 'a) t -> int -> 'a
(** [get_dyn a i] returns the [i]-th element in array [a].
    @raise Invalid_argument if [i] is not a valid index
    (i.e., [i < 0 || i >= n]).
*)

val set_dyn : ('n, 'a) t -> int -> 'a -> unit
(** [set_dyn a i x] destructively assigns [x] to the [i]-th element in array
    [a].
    @raise Invalid_argument if [i] is not a valid index
    (i.e., [i < 0 || i >= n]).
*)

val unsafe_get : ('n, 'a) t -> int -> 'a
(** Like [get_dyn], but size checking is not always performed. *)

val unsafe_set : ('n, 'a) t -> int -> 'a -> unit
(** Like [set_dyn], but size checking is not always performed. *)

val replace_dyn : ('n, 'a) t -> int -> ('a -> 'a) -> unit
(** [replace_dyn a i f] is [set_dyn a i (f (get_dyn a i))]. *)

val copy : ('n, 'a) t -> ('n, 'a) t
(** Returns a shallow copy of the given array. *)

val append : ('m, 'a) t -> ('n, 'a) t -> (('m, 'n) Slap_size.add, 'a) t
(** Concatenate two given arrays. *)

val concat : ('n, 'a) t list -> (module ARRAY with type elt = 'a)
(** Concatenate arrays in the given list. *)

module Concat (X : sig
                 type n
                 type elt
                 val value : (n, elt) t list
               end) : ARRAY with type elt = X.elt
(** A functor version of {!Slap_array.concat}. *)

(** {2 Iterators} *)

val map : ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t

val mapi : (int -> 'a -> 'b) -> ('n, 'a) t -> ('n, 'b) t

val fold_left :
  ('accum -> 'a -> 'accum) -> 'accum -> ('n, 'a) t -> 'accum

val fold_lefti :
  (int -> 'accum -> 'a -> 'accum) -> 'accum -> ('n, 'a) t -> 'accum

val fold_right :
  ('a -> 'accum -> 'accum) -> ('n, 'a) t -> 'accum -> 'accum

val fold_righti :
  (int -> 'a -> 'accum -> 'accum) -> ('n, 'a) t -> 'accum -> 'accum

val iter : ('a -> unit) -> ('n, 'a) t -> unit

val iteri : (int -> 'a -> unit) -> ('n, 'a) t -> unit

val replace_all : ('n, 'a) t -> ('a -> 'a) -> unit

val replace_alli : ('n, 'a) t -> (int -> 'a -> 'a) -> unit

(** {2 Iterators on two sized arrays} *)

val map2 :
  ('a -> 'b -> 'c) ->
  ('n, 'a) t ->
  ('n, 'b) t -> ('n, 'c) t

val mapi2 :
  (int -> 'a -> 'b -> 'c) ->
  ('n, 'a) t ->
  ('n, 'b) t -> ('n, 'c) t

val fold_left2 :
  ('accum -> 'a -> 'b -> 'accum) ->
  'accum ->
  ('n, 'a) t ->
  ('n, 'b) t -> 'accum

val fold_lefti2 :
  (int -> 'accum -> 'a -> 'b -> 'accum) ->
  'accum ->
  ('n, 'a) t ->
  ('n, 'b) t -> 'accum

val fold_right2 :
  ('a -> 'b -> 'accum -> 'accum) ->
  ('n, 'a) t ->
  ('n, 'b) t ->
  'accum -> 'accum

val fold_righti2 :
  (int -> 'a -> 'b -> 'accum -> 'accum) ->
  ('n, 'a) t ->
  ('n, 'b) t ->
  'accum -> 'accum

val iter2 :
  ('a -> 'b -> unit) ->
  ('n, 'a) t ->
  ('n, 'b) t -> unit

val iteri2 :
  (int -> 'a -> 'b -> unit) ->
  ('n, 'a) t ->
  ('n, 'b) t -> unit

(** {2 Iterators on three sized arrays} *)

val map3 :
  ('a -> 'b -> 'c -> 'd) ->
  ('n, 'a) t ->
  ('n, 'b) t ->
  ('n, 'c) t -> ('n, 'd) t

val mapi3 :
  (int -> 'a -> 'b -> 'c -> 'd) ->
  ('n, 'a) t ->
  ('n, 'b) t ->
  ('n, 'c) t -> ('n, 'd) t

val fold_left3 :
  ('accum -> 'a -> 'b -> 'c -> 'accum) ->
  'accum ->
  ('n, 'a) t ->
  ('n, 'b) t ->
  ('n, 'c) t -> 'accum

val fold_lefti3 :
  (int -> 'accum -> 'a -> 'b -> 'c -> 'accum) ->
  'accum ->
  ('n, 'a) t ->
  ('n, 'b) t ->
  ('n, 'c) t -> 'accum

val fold_right3 :
  ('a -> 'b -> 'c -> 'accum -> 'accum) ->
  ('n, 'a) t ->
  ('n, 'b) t ->
  ('n, 'c) t ->
  'accum -> 'accum

val fold_righti3 :
  (int -> 'a -> 'b -> 'c -> 'accum -> 'accum) ->
  ('n, 'a) t ->
  ('n, 'b) t ->
  ('n, 'c) t ->
  'accum -> 'accum

val iter3 :
  ('a -> 'b -> 'c -> unit) ->
  ('n, 'a) t ->
  ('n, 'b) t ->
  ('n, 'c) t -> unit

val iteri3 :
  (int -> 'a -> 'b -> 'c -> unit) ->
  ('n, 'a) t ->
  ('n, 'b) t ->
  ('n, 'c) t -> unit

(** {2 Scanning} *)

val for_all : ('a -> bool) -> ('n, 'a) t -> bool

val exists : ('a -> bool) -> ('n, 'a) t -> bool

val for_all2 : ('a -> 'b -> bool) -> ('n, 'a) t -> ('n, 'b) t -> bool

val exists2 : ('a -> 'b -> bool) -> ('n, 'a) t -> ('n, 'b) t -> bool

(** {2 Conversion} *)

val to_array : ('n, 'a) t -> 'a array
(** [to_array sa] converts sized array [sa] into an ordinary array. *)

val of_array_dyn : 'n Slap_size.t -> 'a array -> ('n, 'a) t
(** [of_array_dyn n a] converts ordinary array [a] of length [n] into a sized
    array.
    @raise Invalid_argument the length of the given array is not equal to [n].
*)

val of_array : 'a array -> (module ARRAY with type elt = 'a)
(** [of_array a] converts ordinary array [a] into a sized array.
    @return a module of signature {!Slap_array.ARRAY} containing a sized array.
*)

module Of_array (X : sig
                   type elt
                   val value : elt array
                 end) :
  ARRAY with type elt = X.elt
(** A functor version of {!Slap_array.of_array}. *)

val to_list : ('n, 'a) t -> 'a list
(** [to_list sa] converts sized array [sa] into an ordinary list. *)

val of_list_dyn : 'n Slap_size.t -> 'a list -> ('n, 'a) t
(** [of_list_dyn n l] converts ordinary list [l] of length [n] into a sized
    array.
    @raise Invalid_argument the length of the given list is not equal to [n].
*)

val of_list : 'a list -> (module ARRAY with type elt = 'a)
(** [of_list l] converts ordinary list [l] into a sized array.
    @return a module of signature {!Slap_array.ARRAY} containing a sized array.
*)

module Of_list (X : sig
                  type elt
                  val value : elt list
                end) :
  ARRAY with type elt = X.elt
(** A functor version of {!Slap_array.of_list}. *)

(** {2 Sorting} *)

val sort : ('a -> 'a -> int) -> ('n, 'a) t -> unit

val stable_sort : ('a -> 'a -> int) -> ('n, 'a) t -> unit

val fast_sort : ('a -> 'a -> int) -> ('n, 'a) t -> unit
