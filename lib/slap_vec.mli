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

open Bigarray

type ('n, 'num, 'prec, 'cnt_or_dsc) t
(** [('n, 'num, 'prec, 'cnt_or_dsc) t] is the type of ['n]-dimensional vector
    whose elements have OCaml type ['num], representation kind ['prec] and
    memory contiguity flag ['cnt_or_dsc].
    The internal implementation is fortran-style one-dimensional big array.
 *)

val cnt : ('n, 'num, 'prec, cnt) t -> ('n, 'num, 'prec, 'cnt) t
(** Recover polymorphism of the fourth type parameter. (type coercion) *)

(** {2 Creation of vectors} *)

val create : ('num, 'prec) kind ->
             'n Size.t -> ('n, 'num, 'prec, 'cnt) t
(** [create kind n]
    @return a fresh [n]-dimensional vector (not initialized).
 *)

val make : ('num, 'prec) kind ->
           'n Size.t ->
           'num -> ('n, 'num, 'prec, 'cnt) t
(** [make kind n a]
    @return a fresh [n]-dimensional vector initialized with [a].
 *)

val init : ('a, 'b) kind ->
           'n Size.t ->
           (int -> 'a) -> ('n, 'a, 'b, 'cnt) t
(** [init kind n f]
    @return a fresh vector [(f 1, ..., f n)] with [n] elements.
 *)

(** {2 Accessors} *)

val kind : ('n, 'num, 'prec, 'cd) t -> ('num, 'prec) kind
(** @return the kind of the given big array. *)

val dim : ('n, 'num, 'prec, 'cd) t -> 'n Size.t
(** [dim x]
    @return the dimension of the vector [x].
 *)

val get_dyn : ('n, 'num, 'prec, 'cd) t -> int -> 'num
(** [get_dyn x i]
    @return the [i]-th element of the vector [x].
 *)

val set_dyn : ('n, 'num, 'prec, 'cd) t -> int -> 'num -> unit
(** [set_dyn x i a] assigns [a] to the [i]-th element of the vector [x].
 *)

val unsafe_get : ('n, 'num, 'prec, 'cd) t -> int -> 'num
(** Like {!Slap.Vec.get_dyn}, but size checking is not always performed. *)

val unsafe_set : ('n, 'num, 'prec, 'cd) t -> int -> 'num -> unit
(** Like {!Slap.Vec.set_dyn}, but size checking is not always performed. *)

val replace_dyn : ('n, 'num, 'prec, 'cd) t ->
                  int ->
                  ('num -> 'num) -> unit
(** [replace_dyn v i f] is [set_dyn v i (f (get_dyn v i))]. *)

(** {2 Iterators} *)

val map : ('y_num, 'y_prec) kind ->
          ('x_num -> 'y_num) ->
          ?y:('n, 'y_num, 'y_prec, 'y_cd) t ->
          ('n, 'x_num, 'x_prec, 'x_cd) t ->
          ('n, 'y_num, 'y_prec, 'y_cd) t
(** [map kind f ?y (x1, ..., xn)] is [(f x1, ..., f xn)].
    @return the vector [y], which is overwritten.
    @param y default = a fresh vector.
 *)

val mapi : ('y_num, 'y_prec) kind ->
           (int -> 'x_num -> 'y_num) ->
           ?y:('n, 'y_num, 'y_prec, 'y_cd) t ->
           ('n, 'x_num, 'x_prec, 'x_cd) t ->
           ('n, 'y_num, 'y_prec, 'y_cd) t
(** [mapi kind f ?y (x1, ..., xn)] is [(f 1 x1, ..., f n xn)] with
    the vector's dimension [n].
    @return the vector [y], which is overwritten.
    @param y default = a fresh vector.
 *)

val fold_left : ('accum -> 'x_num -> 'accum) ->
                'accum ->
                ('n, 'x_num, 'prec, 'cd) t -> 'accum
(** [fold_left f init (x1, x2, ..., xn)] is
    [f (... (f (f init x1) x2) ...) xn].
 *)

val fold_lefti : (int -> 'accum -> 'num -> 'accum) ->
                 'accum ->
                 ('n, 'num, 'prec, 'cd) t -> 'accum
(** [fold_lefti f init (x1, x2, ..., xn)] is
    [f n (... (f 2 (f 1 init x1) x2) ...) xn] with the vector's dimension [n].
 *)

val fold_right : ('num -> 'accum -> 'accum) ->
                 ('n, 'num, 'prec, 'cd) t ->
                 'accum -> 'accum
(** [fold_right f (x1, x2, ..., xn) init] is
    [f x1 (f x2 (... (f xn init) ...))].
 *)

val fold_righti : (int -> 'num -> 'accum -> 'accum) ->
                  ('n, 'num, 'prec, 'cd) t ->
                  'accum -> 'accum
(** [fold_righti f (x1, x2, ..., xn) init] is
    [f 1 x1 (f 2 x2 (... (f n xn init) ...))] with the vector's dimension [n].
 *)

val replace_all : ('n, 'num, 'prec, 'cd) t ->
                  ('num -> 'num) -> unit
(** [replace_all x f] modifies the vector [x] in place
    -- the [i]-th element [xi] of [x] will be set to [f xi].
 *)

val replace_alli : ('n, 'num, 'prec, 'cd) t ->
                   (int -> 'num -> 'num) -> unit
(** [replace_alli x f] modifies the vector [x] in place
    -- the [i]-th element [xi] of [x] will be set to [f i xi].
 *)

val iter : ('num -> unit) ->
           ('n, 'num, 'prec, 'cd) t -> unit
(** [iter f (x1, x2, ..., xn)] is [f x1; f x2; ...; f xn]. *)

val iteri : (int -> 'num -> unit) ->
            ('n, 'num, 'prec, 'cd) t -> unit
(** [iteri f (x1, x2, ..., xn)] is [f 1 x1; f 2 x2; ...; f n xn]. *)

(** {2 Basic operations} *)

val copy : ?y:('n, 'num, 'prec, 'y_cd) t ->
           ('n, 'num, 'prec, 'x_cd) t -> ('n, 'num, 'prec, 'y_cd) t
(** [copy ?y x] copies the vector [x] to the vector [y].
    @return the vector [y], which is overwritten.
    @param y default = a fresh vector.
 *)

val fill : ('n, 'num, 'prec, 'cd) t -> 'num -> unit
(** Fill the given vector with the given value. *)

val append : ('m, 'num, 'prec, 'x_cd) t ->
             ('n, 'num, 'prec, 'y_cd) t ->
             (('m, 'n) Size.add, 'num, 'prec, 'cnt) t
(** Concatenate two vectors. *)

(** {2 Type conversion} *)

val to_array : ('n, 'num, 'prec, 'cd) t -> 'num array
(** [to_array x]
    @return the array of all the elements of the vector [x].
 *)

val of_array_dyn : ('num, 'prec) kind ->
                   'n Size.t ->
                   'num array -> ('n, 'num, 'prec, 'cnt) t
(** [of_array_dyn kind n [|a1; ...; an|]]
    @raise Invalid_argument the length of the given array is not equal to [n].
    @return a fresh vector [(a1, ..., an)].
 *)

val to_list : ('n, 'num, 'prec, 'cd) t -> 'num list
(** [to_list x]
    @return the list of all the elements of the vector [x].
 *)

val of_list_dyn : ('num, 'prec) kind ->
                  'n Size.t ->
                  'num list -> ('n, 'num, 'prec, 'cnt) t
(** [of_list_dyn kind n [a1; ...; an]]
    @raise Invalid_argument the length of the given list is not equal to [n].
    @return a fresh vector [(a1, ..., an)].
 *)

(** {2 Subvectors} *)

val subcntvec_dyn : 'm Size.t ->
                 ?ofsx:int ->
                 ('n, 'num, 'prec, cnt) t ->
                 ('m, 'num, 'prec, 'cnt) t
(** [subcntvec_dyn m ?ofsx x]
    @return a subvector of the contiguous vector [x].
    The [i]-th element of it refers [(ofsx+i-1)]-th element of [x].
    The data are shared.
    @param ofsx default = 1
    @raise Invalid_argument [m] and [ofsx] do not designate a valid subvector of
    [x].
 *)

val subdscvec_dyn : 'm Size.t ->
                    ?ofsx:int ->
                    ?incx:int ->
                    ('n, 'num, 'prec, 'cd) t ->
                    ('m, 'num, 'prec, dsc) t
(** [subdscvec_dyn m ?ofsx ?incx x]
    @return a subvector of the vector [x].
    The [i]-th element of it refers [(ofsx+(i-1)*incx)]-th element of [x].
    The data are shared.
    @param ofs default = 1
    @param inc default = 1
    @raise Invalid_argument [m], [ofsx] and [incx] do not designate
    a valid subvector of [x].
 *)

val subvec_dyn : 'm Size.t ->
                 ?ofsx:int ->
                 ?incx:int ->
                 ('n, 'num, 'prec, 'cd) t ->
                 ('m, 'num, 'prec, dsc) t
(** An alias of [subdscvec_dyn]. *)
