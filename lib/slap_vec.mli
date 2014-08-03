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

type (+'n, 'num, 'prec, +'cnt_or_dsc) t
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

(** {2 Iterators on two vectors} *)

val map2 : ('z_num, 'z_prec) kind ->
           ('x_num -> 'y_num -> 'z_num) ->
           ?z:('n, 'z_num, 'z_prec, 'z_cd) t ->
           ('n, 'x_num, 'x_prec, 'x_cd) t ->
           ('n, 'y_num, 'y_prec, 'y_cd) t ->
           ('n, 'z_num, 'z_prec, 'z_cd) t
(** [map2 kind f ?z (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(f x1 y1, f x2 y2, ..., f xn yn)].
    @return the vector [z], which is overwritten.
    @param z default = a fresh vector.
 *)

val mapi2 : ('z_num, 'z_prec) kind ->
            (int -> 'x_num -> 'y_num -> 'z_num) ->
            ?z:('n, 'z_num, 'z_prec, 'z_cd) t ->
            ('n, 'x_num, 'x_prec, 'x_cd) t ->
            ('n, 'y_num, 'y_prec, 'y_cd) t ->
            ('n, 'z_num, 'z_prec, 'z_cd) t
(** [mapi2 kind f ?z (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(f 1 x1 y1, f 2 x2 y2, ..., f n xn yn)] with the vectors' dimension [n].
    @return the vector [z], which is overwritten.
    @param z default = a fresh vector.
 *)

val fold_left2 : ('accum -> 'x_num -> 'y_num -> 'accum) ->
                 'accum ->
                 ('n, 'x_num, 'x_prec, 'x_cd) t ->
                 ('n, 'y_num, 'y_prec, 'y_cd) t -> 'accum
(** [fold_left2 f init (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f (... (f (f init x1 y1) x2 y2) ...) xn yn].
 *)

val fold_lefti2 : (int -> 'accum -> 'x_num -> 'y_num -> 'accum) ->
                  'accum ->
                  ('n, 'x_num, 'x_prec, 'x_cd) t ->
                  ('n, 'y_num, 'y_prec, 'y_cd) t -> 'accum
(** [fold_lefti2 f init (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f n (... (f 2 (f 1 init x1 y1) x2 y2) ...) xn yn] with the vectors'
    dimension [n].
 *)

val fold_right2 : ('x_num -> 'y_num -> 'accum -> 'accum) ->
                  ('n, 'x_num, 'x_prec, 'x_cd) t ->
                  ('n, 'y_num, 'y_prec, 'y_cd) t ->
                  'accum -> 'accum
(** [fold_righti2 f (x1, x2, ..., xn) (y1, y2, ..., yn) init] is
    [f x1 y1 (f x2 y2 (... (f xn yn init) ...))].
 *)

val fold_righti2 : (int -> 'x_num -> 'y_num -> 'accum -> 'accum) ->
                   ('n, 'x_num, 'x_prec, 'x_cd) t ->
                   ('n, 'y_num, 'y_prec, 'y_cd) t ->
                   'accum -> 'accum
(** [fold_righti2 f (x1, x2, ..., xn) (y1, y2, ..., yn) init] is
    [f 1 x1 y1 (f 2 x2 y2 (... (f n xn yn init) ...))] with the vectors'
    dimension [n].
 *)

val iter2 : ('x_num -> 'y_num -> unit) ->
            ('n, 'x_num, 'x_prec, 'x_cd) t ->
            ('n, 'y_num, 'y_prec, 'y_cd) t -> unit
(** [iter2 f (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f x1 y1; f x2 y2; ...; f xn yn].
 *)

val iteri2 : (int -> 'x_num -> 'y_num -> unit) ->
            ('n, 'x_num, 'x_prec, 'x_cd) t ->
            ('n, 'y_num, 'y_prec, 'y_cd) t -> unit
(** [iteri2 f (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f 1 x1 y1; f 2 x2 y2; ...; f n xn yn].
 *)

(** {2 Iterators on three vectors} *)

val map3 : ('w_num, 'w_prec) kind ->
           ('x_num -> 'y_num -> 'z_num -> 'w_num) ->
           ?w:('n, 'w_num, 'w_prec, 'w_cd) t ->
           ('n, 'x_num, 'x_prec, 'x_cd) t ->
           ('n, 'y_num, 'y_prec, 'y_cd) t ->
           ('n, 'z_num, 'z_prec, 'z_cd) t ->
           ('n, 'w_num, 'w_prec, 'w_cd) t
(** [map3 kind f ?w (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)] is
    [(f x1 y1 z1, f x2 y2 z2, ..., f xn yn zn)].
    @return the vector [w], which is overwritten.
    @param w default = a fresh vector.
 *)

val mapi3 : ('w_num, 'w_prec) kind ->
            (int -> 'x_num -> 'y_num -> 'z_num -> 'w_num) ->
            ?w:('n, 'w_num, 'w_prec, 'w_cd) t ->
            ('n, 'x_num, 'x_prec, 'x_cd) t ->
            ('n, 'y_num, 'y_prec, 'y_cd) t ->
            ('n, 'z_num, 'z_prec, 'z_cd) t ->
            ('n, 'w_num, 'w_prec, 'w_cd) t
(** [mapi3 kind f ?w (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)] is
    [(f 1 x1 y1 z1, f 2 x2 y2 z2, ..., f n xn yn zn)] with the vectors'
    dimension [n].
    @return the vector [w], which is overwritten.
    @param w default = a fresh vector.
 *)

val fold_left3 : ('accum -> 'x_num -> 'y_num -> 'z_num -> 'accum) ->
                 'accum ->
                 ('n, 'x_num, 'x_prec, 'x_cd) t ->
                 ('n, 'y_num, 'y_prec, 'y_cd) t ->
                 ('n, 'z_num, 'z_prec, 'z_cd) t -> 'accum
(** [fold_left3 f init (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)]
    is [f (... (f (f init x1 y1 z1) x2 y2 z2) ...) xn yn zn].
 *)

val fold_lefti3 : (int -> 'accum -> 'x_num -> 'y_num -> 'z_num -> 'accum) ->
                  'accum ->
                  ('n, 'x_num, 'x_prec, 'x_cd) t ->
                  ('n, 'y_num, 'y_prec, 'y_cd) t ->
                  ('n, 'z_num, 'z_prec, 'z_cd) t -> 'accum
(** [fold_lefti3 f init (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)]
    is [f n (... (f 2 (f 1 init x1 y1 z1) x2 y2 z2) ...) xn yn zn] with the
    vectors' dimension [n].
 *)

val fold_right3 : ('x_num -> 'y_num -> 'z_num -> 'accum -> 'accum) ->
                  ('n, 'x_num, 'x_prec, 'x_cd) t ->
                  ('n, 'y_num, 'y_prec, 'y_cd) t ->
                  ('n, 'z_num, 'z_prec, 'z_cd) t ->
                  'accum -> 'accum
(** [fold_right3 f (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn) init]
    is [f x1 y1 z1 (f x2 y2 z2 (... (f xn yn zn init) ...))].
 *)

val fold_righti3 : (int -> 'x_num -> 'y_num -> 'z_num -> 'accum -> 'accum) ->
                   ('n, 'x_num, 'x_prec, 'x_cd) t ->
                   ('n, 'y_num, 'y_prec, 'y_cd) t ->
                   ('n, 'z_num, 'z_prec, 'z_cd) t ->
                   'accum -> 'accum
(** [fold_righti3 f (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn) init]
    is [f 1 x1 y1 z1 (f 2 x2 y2 z2 (... (f n xn yn zn init) ...))]
    with the vectors' dimension [n].
 *)

val iter3 : ('x_num -> 'y_num -> 'z_num -> unit) ->
            ('n, 'x_num, 'x_prec, 'x_cd) t ->
            ('n, 'y_num, 'y_prec, 'y_cd) t ->
            ('n, 'z_num, 'z_prec, 'z_cd) t -> unit
(** [iter3 f (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)] is
    [f x1 y1 z1; f x2 y2 z2; ...; f xn yn zn].
 *)

val iteri3 : (int -> 'x_num -> 'y_num -> 'z_num -> unit) ->
             ('n, 'x_num, 'x_prec, 'x_cd) t ->
             ('n, 'y_num, 'y_prec, 'y_cd) t ->
             ('n, 'z_num, 'z_prec, 'z_cd) t -> unit
(** [iteri3 f (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)] is
    [f 1 x1 y1 z1; f 2 x2 y2 z2; ...; f n xn yn zn].
 *)

(** {2 Scanning} *)

val for_all : ('num -> bool) ->
             ('n, 'num, 'prec, 'cd) t -> bool
(** [for_all p (x1, x2, ..., xn)] is [(p x1) && (p x2) && ... && (p xn)].
    @return [true] if and only if all elements of the given vector satisfy
    the predicate [p].
 *)

val exists : ('num -> bool) ->
             ('n, 'num, 'prec, 'cd) t -> bool
(** [exists p (x1, x2, ..., xn)] is [(p x1) || (p x2) || ... || (p xn)].
    @return [true] if and only if at least one element of the given vector
    satisfies the predicate [p].
 *)

val for_all2 : ('x_num -> 'y_num -> bool) ->
               ('n, 'x_num, 'x_prec, 'x_cd) t ->
               ('n, 'y_num, 'y_prec, 'y_cd) t -> bool
(** [for_all2 p (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(p x1 y1) && (p x2 y2) && ... && (p xn yn)].
    @return [true] if and only if all elements of the given two vectors
    satisfy the predicate [p].
 *)

val exists2 : ('x_num -> 'y_num -> bool) ->
              ('n, 'x_num, 'x_prec, 'x_cd) t ->
              ('n, 'y_num, 'y_prec, 'y_cd) t -> bool
(** [exists2 p (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(p x1 y1) || (p x2 y2) || ... || (p xn yn)].
    @return [true] if and only if at least one pair of elements of the given two
    vectors satisfies the predicate [p].
 *)

val mem : ?equal:('num -> 'num -> bool) ->
          'num -> ('n, 'num, 'prec, 'cd) t -> bool
(** [mem ?equal a v]
    @return [true] if and only if [a] is equal to an element of [v].
    @param equal default = [(=)] (polymorphic compare)
 *)

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

val shared_rev : ('n, 'num, 'prec, 'cd) t -> ('n, 'num, 'prec, 'cd) t
(** [shared_rev (x1, x2, ..., xn)]
    @return reversed vector [(xn, ..., x2, x1)]. The data are shared.
 *)

val rev : ('n, 'num, 'prec, 'cd) t -> ('n, 'num, 'prec, 'cd) t
(** [rev (x1, x2, ..., xn)]
    @return reversed vector [(xn, ..., x2, x1)]. The data are NOT shared.
 *)

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

val to_bigarray : ('n, 'num, 'prec, 'cd) t ->
                  ('num, 'prec, fortran_layout) Array1.t
(** [to_bigarray x]
    @return the big array of all the elements of the vector [x].
 *)

val of_bigarray_dyn : ?share:bool ->
                      'n Size.t ->
                      ('num, 'prec, fortran_layout) Array1.t ->
                      ('n, 'num, 'prec, 'cnt) t
(** [of_bigarray_dyn ?share n ba]
    @raise Invalid_argument the length of the given big array is not equal to
          [n].
    @return a fresh vector of all the elements of big array [ba].
    @param share [true] if data are shared. (default = [false])
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
