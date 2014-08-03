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

module type CNTVEC =
  sig
    type n
    (** A generative phantom type. *)

    val value : (n, 'cnt) vec
    (** A dynamically-sized contiguous vector with type like
        [exists n. (n, 'cnt) vec]. *)
  end
(** The signature of modules containing dynamically-sized contiguous vectors. *)

module type DSCVEC =
  sig
    type n
    (** A generative phantom type. *)

    val value : (n, dsc) vec
    (** A dynamically-sized discrete vector with type like
        [exists n. (n, dsc) vec]. *)
  end
(** The signature of modules containing dynamically-sized discrete vectors. *)

(** {2 Creation of vectors} *)

val empty : (Size.z, 'cnt) vec
(** An empty vector. *)

val create : 'n Size.t -> ('n, 'cnt) vec
(** [create n]
    @return a fresh [n]-dimensional vector (not initialized).
 *)

val make : 'n Size.t -> num_type -> ('n, 'cnt) vec
(** [make n a]
    @return a fresh [n]-dimensional vector initialized with [a].
 *)

val make0 : 'n Size.t -> ('n, 'cnt) vec
(** [zeros n]
    @return a fresh [n]-dimensional vector initialized with [0].
 *)

val make1 : 'n Size.t -> ('n, 'cnt) vec
(** [make1 n]
    @return a fresh [n]-dimensional vector initialized with [1].
 *)

val init : 'n Size.t ->
           (int -> num_type) -> ('n, 'cnt) vec
(** [init n f]
    @return a fresh vector [(f 1, ..., f n)] with [n] elements.
 *)

(** {2 Accessors} *)

val dim : ('n, 'cd) vec -> 'n Size.t
(** [dim x]
    @return the dimension of the vector [x].
 *)

val get_dyn : ('n, 'cd) vec -> int -> num_type
(** [get_dyn x i]
    @return the [i]-th element of the vector [x].
 *)

val set_dyn : ('n, 'cd) vec -> int -> num_type -> unit
(** [set_dyn x i a] assigns [a] to the [i]-th element of the vector [x].
 *)

val unsafe_get : ('n, 'cd) vec -> int -> num_type
(** Like {!Slap.Vec.get_dyn}, but size checking is not always performed. *)

val unsafe_set : ('n, 'cd) vec -> int -> num_type -> unit
(** Like {!Slap.Vec.set_dyn}, but size checking is not always performed. *)

val replace_dyn : ('n, 'cd) vec ->
                  int ->
                  (num_type -> num_type) -> unit
(** [replace_dyn v i f] is [set_dyn v i (f (get_dyn v i))]. *)

(** {2 Basic operations} *)

val copy : ?y:('n, 'y_cd) vec -> ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [copy ?y x] copies the vector [x] to the vector [y] with the BLAS-1
    function [[sdcz]copy].
    @return the vector [y], which is overwritten.
    @param y default = a fresh vector.
 *)

val fill : ('n, 'cd) vec -> num_type -> unit
(** Fill the given vector with the given value. *)

val append : ('m, 'x_cd) vec -> ('n, 'y_cd) vec ->
             (('m, 'n) Size.add, 'cnt) vec
(** Concatenate two vectors. *)

val shared_rev : ('n, 'cd) vec -> ('n, 'cd) vec
(** [shared_rev (x1, x2, ..., xn)]
    @return reversed vector [(xn, ..., x2, x1)]. The data are shared.
 *)

val rev : ('n, 'cd) vec -> ('n, 'cd) vec
(** [rev (x1, x2, ..., xn)]
    @return reversed vector [(xn, ..., x2, x1)]. The data are NOT shared.
 *)

(** {2 Type conversion} *)

val to_array : ('n, 'cd) vec -> num_type array
(** [to_array x]
    @return the array of all the elements of the vector [x].
 *)

val of_array_dyn : 'n Size.t ->
                   num_type array -> ('n, 'cnt) vec
(** [of_array_dyn n [|a1; ...; an|]]
    @raise Invalid_argument the length of the given array is not equal to [n].
    @return a fresh vector [(a1, ..., an)].
 *)

val of_array : num_type array -> (module CNTVEC)
(** [module V = (val of_array [|a1; ...; an|] : CNTVEC)]
    @return module [V] containing the vector [V.value] (= [(a1, ..., an)]) that
    has the type [(V.n, 'cnt) vec] with a generative phantom type [V.n] as a
    package of an existential quantified sized type like
    [exists n. (n, 'cnt) vec].
 *)

module Of_array (X : sig val value : num_type array end) : CNTVEC
(** A functor version of [of_array]. *)

val to_list : ('n, 'cd) vec -> num_type list
(** [to_list x]
    @return the list of all the elements of the vector [x].
 *)

val of_list_dyn : 'n Size.t ->
                  num_type list -> ('n, 'cnt) vec
(** [of_list_dyn n [a1; ...; an]]
    @raise Invalid_argument the length of the given list is not equal to [n].
    @return a fresh vector [(a1, ..., an)].
 *)

val of_list : num_type list -> (module CNTVEC)
(** [module V = (val of_list [a1; ...; an] : CNTVEC)]
    @return module [V] containing the vector [V.value] (= [(a1, ..., an)]) that
    has the type [(V.n, 'cnt) vec] with a generative phantom type [V.n] as a
    package of an existential quantified sized type like
    [exists n. (n, 'cnt) vec].
 *)

module Of_list (X : sig val value : num_type list end) : CNTVEC
(** A functor version of [of_list]. *)

val to_bigarray : ('n, 'cd) vec ->
                  (num_type, prec, fortran_layout) Array1.t
(** [to_bigarray x]
    @return the big array of all the elements of the vector [x].
 *)

val of_bigarray_dyn : ?share:bool ->
                      'n Size.t ->
                      (num_type, prec, fortran_layout) Array1.t ->
                      ('n, 'cnt) vec
(** [of_bigarray_dyn ?share n ba]
    @raise Invalid_argument the length of the given big array is not equal to
          [n].
    @return a fresh vector of all the elements of big array [ba].
    @param share [true] if data are shared. (default = [false])
 *)

val of_bigarray : ?share:bool ->
                  (num_type, prec, fortran_layout) Array1.t ->
                  (module CNTVEC)
(** [module V = (val of_bigarray ?share n ba : CNTVEC)]
    @return module [V] containing the vector [V.value] (of all elements of big
    array [ba]) that has the type [(V.n, 'cnt) vec] with a generative phantom
    type [V.n] as a package of an existential quantified sized type like
    [exists n. (n, 'cnt) vec].
    @param share [true] if data are shared. (default = [false])
 *)

module Of_bigarray (X : sig
                          val share : bool
                          val value : (num_type, prec, fortran_layout) Array1.t
                        end) : CNTVEC
(** A functor version of [of_bigarray]. *)

(** {2 Iterators} *)

val map : (num_type -> num_type) ->
          ?y:('n, 'y_cd) vec ->
          ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [map f ?y (x1, ..., xn)] is [(f x1, ..., f xn)].
   @return the vector [y], which is overwritten.
   @param y default = a fresh vector.
 *)

val mapi : (int -> num_type -> num_type) ->
           ?y:('n, 'y_cd) vec ->
           ('n, 'x_cd) vec ->
           ('n, 'y_cd) vec
(** [mapi f ?y (x1, ..., xn)] is [(f 1 x1, ..., f n xn)] with
   the vector's dimension [n].
   @return the vector [y], which is overwritten.
   @param y default = a fresh vector.
 *)

val fold_left : ('accum -> num_type -> 'accum) ->
                'accum ->
                ('n, 'cd) vec -> 'accum
(** [fold_left f init (x1, x2, ..., xn)] is
   [f (... (f (f init x1) x2) ...) xn].
 *)

val fold_lefti : (int -> 'accum -> num_type -> 'accum) ->
                 'accum ->
                 ('n, 'cd) vec -> 'accum
(** [fold_lefti f init (x1, x2, ..., xn)] is
   [f n (... (f 2 (f 1 init x1) x2) ...) xn] with the vector's dimension [n].
 *)

val fold_right : (num_type -> 'accum -> 'accum) ->
                 ('n, 'cd) vec ->
                 'accum -> 'accum
(** [fold_right f (x1, x2, ..., xn) init] is
   [f x1 (f x2 (... (f xn init) ...))].
 *)

val fold_righti : (int -> num_type -> 'accum -> 'accum) ->
                  ('n, 'cd) vec ->
                  'accum -> 'accum
(** [fold_righti f (x1, x2, ..., xn) init] is
   [f 1 x1 (f 2 x2 (... (f n xn init) ...))] with the vector's dimension [n].
 *)

val replace_all : ('n, 'cd) vec ->
                  (num_type -> num_type) -> unit
(** [replace_all x f] modifies the vector [x] in place
   -- the [i]-th element [xi] of [x] will be set to [f xi].
 *)

val replace_alli : ('n, 'cd) vec ->
                   (int -> num_type -> num_type) -> unit
(** [replace_alli x f] modifies the vector [x] in place
   -- the [i]-th element [xi] of [x] will be set to [f i xi].
 *)

val iter : (num_type -> unit) ->
           ('n, 'cd) vec -> unit
(** [iter f (x1, x2, ..., xn)] is [f x1; f x2; ...; f xn].
 *)

val iteri : (int -> num_type -> unit) ->
            ('n, 'cd) vec -> unit
(** [iteri f (x1, x2, ..., xn)] is [f 1 x1; f 2 x2; ...; f n xn].
 *)

(** {2 Iterators on two vectors} *)

val map2 : (num_type -> num_type -> num_type) ->
           ?z:('n, 'z_cd) vec ->
           ('n, 'x_cd) vec ->
           ('n, 'y_cd) vec ->
           ('n, 'z_cd) vec
(** [map2 f ?z (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(f x1 y1, f x2 y2, ..., f xn yn)].
    @return the vector [z], which is overwritten.
    @param z default = a fresh vector.
 *)

val mapi2 : (int -> num_type -> num_type -> num_type) ->
            ?z:('n, 'z_cd) vec ->
            ('n, 'x_cd) vec ->
            ('n, 'y_cd) vec ->
            ('n, 'z_cd) vec
(** [mapi2 f ?z (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(f 1 x1 y1, f 2 x2 y2, ..., f n xn yn)] with the vectors' dimension [n].
    @return the vector [z], which is overwritten.
    @param z default = a fresh vector.
 *)

val fold_left2 : ('accum -> num_type -> num_type -> 'accum) ->
                 'accum ->
                 ('n, 'x_cd) vec ->
                 ('n, 'y_cd) vec -> 'accum
(** [fold_left2 f init (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f (... (f (f init x1 y1) x2 y2) ...) xn yn].
 *)

val fold_lefti2 : (int -> 'accum -> num_type -> num_type -> 'accum) ->
                  'accum ->
                  ('n, 'x_cd) vec ->
                  ('n, 'y_cd) vec -> 'accum
(** [fold_lefti2 f init (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f n (... (f 2 (f 1 init x1 y1) x2 y2) ...) xn yn] with the vectors'
    dimension [n].
 *)

val fold_right2 : (num_type -> num_type -> 'accum -> 'accum) ->
                  ('n, 'x_cd) vec ->
                  ('n, 'y_cd) vec ->
                  'accum -> 'accum
(** [fold_righti2 f (x1, x2, ..., xn) (y1, y2, ..., yn) init] is
    [f x1 y1 (f x2 y2 (... (f xn yn init) ...))].
 *)

val fold_righti2 : (int -> num_type -> num_type -> 'accum -> 'accum) ->
                   ('n, 'x_cd) vec ->
                   ('n, 'y_cd) vec ->
                   'accum -> 'accum
(** [fold_righti2 f (x1, x2, ..., xn) (y1, y2, ..., yn) init] is
    [f 1 x1 y1 (f 2 x2 y2 (... (f n xn yn init) ...))] with the vectors'
    dimension [n].
 *)

val iter2 : (num_type -> num_type -> unit) ->
            ('n, 'x_cd) vec ->
            ('n, 'y_cd) vec -> unit
(** [iter2 f (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f x1 y1; f x2 y2; ...; f xn yn].
 *)

val iteri2 : (int -> num_type -> num_type -> unit) ->
            ('n, 'x_cd) vec ->
            ('n, 'y_cd) vec -> unit
(** [iteri2 f (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f 1 x1 y1; f 2 x2 y2; ...; f n xn yn].
 *)

(** {2 Iterators on three vectors} *)

val map3 : (num_type -> num_type -> num_type -> num_type) ->
           ?w:('n, 'w_cd) vec ->
           ('n, 'x_cd) vec ->
           ('n, 'y_cd) vec ->
           ('n, 'z_cd) vec ->
           ('n, 'w_cd) vec
(** [map3 f ?w (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)] is
    [(f x1 y1 z1, f x2 y2 z2, ..., f xn yn zn)].
    @return the vector [w], which is overwritten.
    @param w default = a fresh vector.
 *)

val mapi3 : (int -> num_type -> num_type -> num_type -> num_type) ->
           ?w:('n, 'w_cd) vec ->
           ('n, 'x_cd) vec ->
           ('n, 'y_cd) vec ->
           ('n, 'z_cd) vec ->
           ('n, 'w_cd) vec
(** [mapi3 f ?w (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)] is
    [(f 1 x1 y1 z1, f 2 x2 y2 z2, ..., f n xn yn zn)] with the vectors'
    dimension [n].
    @return the vector [w], which is overwritten.
    @param w default = a fresh vector.
 *)

val fold_left3 : ('accum -> num_type -> num_type -> num_type -> 'accum) ->
                 'accum ->
                 ('n, 'x_cd) vec ->
                 ('n, 'y_cd) vec ->
                 ('n, 'z_cd) vec -> 'accum
(** [fold_left3 f init (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)]
    is [f (... (f (f init x1 y1 z1) x2 y2 z2) ...) xn yn zn].
 *)

val fold_lefti3 : (int -> 'accum -> num_type -> num_type -> num_type->'accum) ->
                 'accum ->
                 ('n, 'x_cd) vec ->
                 ('n, 'y_cd) vec ->
                 ('n, 'z_cd) vec -> 'accum
(** [fold_lefti3 f init (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)]
    is [f n (... (f 2 (f 1 init x1 y1 z1) x2 y2 z2) ...) xn yn zn] with the
    vectors' dimension [n].
 *)

val fold_right3 : (num_type -> num_type -> num_type -> 'accum -> 'accum) ->
                  ('n, 'x_cd) vec ->
                  ('n, 'y_cd) vec ->
                  ('n, 'z_cd) vec ->
                  'accum -> 'accum
(** [fold_right3 f (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn) init]
    is [f x1 y1 z1 (f x2 y2 z2 (... (f xn yn zn init) ...))].
 *)

val fold_righti3 : (int -> num_type -> num_type -> num_type -> 'accum->'accum)->
                  ('n, 'x_cd) vec ->
                  ('n, 'y_cd) vec ->
                  ('n, 'z_cd) vec ->
                  'accum -> 'accum
(** [fold_righti3 f (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn) init]
    is [f 1 x1 y1 z1 (f 2 x2 y2 z2 (... (f n xn yn zn init) ...))]
    with the vectors' dimension [n].
 *)

val iter3 : (num_type -> num_type -> num_type -> unit) ->
            ('n, 'x_cd) vec ->
            ('n, 'y_cd) vec ->
            ('n, 'z_cd) vec -> unit
(** [iter3 f (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)] is
    [f x1 y1 z1; f x2 y2 z2; ...; f xn yn zn].
 *)

val iteri3 : (int -> num_type -> num_type -> num_type -> unit) ->
             ('n, 'x_cd) vec ->
             ('n, 'y_cd) vec ->
             ('n, 'z_cd) vec -> unit
(** [iteri3 f (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)] is
    [f 1 x1 y1 z1; f 2 x2 y2 z2; ...; f n xn yn zn].
 *)

(** {2 Scanning} *)

val for_all : (num_type -> bool) -> ('n, 'cd) vec -> bool
(** [for_all p (x1, x2, ..., xn)] is [(p x1) && (p x2) && ... && (p xn)].
    @return [true] if and only if all elements of the given vector satisfy
    the predicate [p].
 *)

val exists : (num_type -> bool) -> ('n, 'cd) vec -> bool
(** [exists p (x1, x2, ..., xn)] is [(p x1) || (p x2) || ... || (p xn)].
    @return [true] if and only if at least one element of the given vector
    satisfies the predicate [p].
 *)

val for_all2 : (num_type -> num_type -> bool) ->
               ('n, 'x_cd) vec ->
               ('n, 'y_cd) vec -> bool
(** [for_all2 p (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(p x1 y1) && (p x2 y2) && ... && (p xn yn)].
    @return [true] if and only if all elements of the given two vectors
    satisfy the predicate [p].
 *)

val exists2 : (num_type -> num_type -> bool) ->
               ('n, 'x_cd) vec ->
               ('n, 'y_cd) vec -> bool
(** [exists2 p (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(p x1 y1) || (p x2 y2) || ... || (p xn yn)].
    @return [true] if and only if at least one pair of elements of the given two
    vectors satisfies the predicate [p].
 *)

(** {2 Arithmetic operations} *)

val max : ('n, 'cd) vec -> num_type

val min : ('n, 'cd) vec -> num_type

val sum : ('n, 'cd) vec -> num_type

val prod : ('n, 'cd) vec -> num_type

val sqr_nrm2 : ?stable:bool -> ('n, 'cd) vec -> float

val ssqr : ?c:num_type -> ('n, 'cd) vec -> num_type

val sort : ?cmp:(num_type -> num_type -> int) ->
           ?decr:bool ->
           ?p:('n, 'p_cd) Common.int_vec ->
           ('n, 'x_cd) vec -> unit

val neg : ?y:('n, 'y_cd) vec -> ('n, 'x_cd) vec -> ('n, 'y_cd) vec

val add : ?z:('n, 'z_cd) vec -> ('n, 'x_cd) vec ->
          ('n, 'y_cd) vec -> ('n, 'z_cd) vec

val sub : ?z:('n, 'z_cd) vec -> ('n, 'x_cd) vec ->
          ('n, 'y_cd) vec -> ('n, 'z_cd) vec

val mul : ?z:('n, 'z_cd) vec -> ('n, 'x_cd) vec ->
          ('n, 'y_cd) vec -> ('n, 'z_cd) vec

val div : ?z:('n, 'z_cd) vec -> ('n, 'x_cd) vec ->
          ('n, 'y_cd) vec -> ('n, 'z_cd) vec

val ssqr_diff : ('n, 'x_cd) vec -> ('n, 'y_cd) vec -> num_type
