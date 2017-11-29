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

val cnt : ('n, cnt) vec -> ('n, 'cnt) vec
(** Recover polymorphism of the fourth type parameter. *)

(** {2 Creation of vectors} *)

val empty : (Slap_size.z, 'cnt) vec
(** An empty vector. *)

val create : 'n Slap_size.t -> ('n, 'cnt) vec
(** [create n]
    @return a fresh [n]-dimensional vector (not initialized).
 *)

val make : 'n Slap_size.t -> num_type -> ('n, 'cnt) vec
(** [make n a]
    @return a fresh [n]-dimensional vector initialized with [a].
 *)

val make0 : 'n Slap_size.t -> ('n, 'cnt) vec
(** [zeros n]
    @return a fresh [n]-dimensional vector initialized with [0].
 *)

val make1 : 'n Slap_size.t -> ('n, 'cnt) vec
(** [make1 n]
    @return a fresh [n]-dimensional vector initialized with [1].
 *)

val init : 'n Slap_size.t -> (int -> num_type) -> ('n, 'cnt) vec
(** [init n f]
    @return a fresh vector [(f 1, ..., f n)] with [n] elements.
 *)

(** {2 Accessors} *)

val dim : ('n, 'cd) vec -> 'n Slap_size.t
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
(** Like [get_dyn], but size checking is not always performed. *)

val unsafe_set : ('n, 'cd) vec -> int -> num_type -> unit
(** Like [set_dyn], but size checking is not always performed. *)

val replace_dyn : ('n, 'cd) vec -> int -> (num_type -> num_type) -> unit
(** [replace_dyn v i f] is [set_dyn v i (f (get_dyn v i))]. *)

(** {2 Basic operations} *)

val cons :
  ?y:('n Slap_size.s, 'y_cd) vec ->
  num_type ->
  ('n, 'x_cd) vec ->
  ('n Slap_size.s, 'y_cd) vec
(** [cons ?y e x] adds element [e] at the beginning of vector [x], and copies
    it into [y].
    @return the vector [y], which is overwritten.
    @param y default = a fresh vector.
    @since 4.0.0 *)

val hd :
  ('n Slap_size.s, 'x_cd) vec -> num_type
(** @return the first element of a given vector. (typesafe)
    @since 4.0.0 *)

val hd_dyn :
  ('n, 'x_cd) vec -> num_type
(** @return the first element of a given vector.
    @since 4.0.0 *)

val last :
  ('n Slap_size.s, 'x_cd) vec -> num_type
(** @return the last element of a given vector. (typesafe)
    @since 4.0.0 *)

val last_dyn :
  ('n, 'x_cd) vec -> num_type
(** @return the last element of a given vector.
    @since 4.0.0 *)

val tl :
  ?share:bool ->
  ('n Slap_size.s, 'x_cd) vec -> ('n, 'x_cd) vec
(** @return a given vector without the first element. (typesafe)
    @param share [true] if a returned subvector refers a given vector
           (default = [false])
    @since 4.0.0 *)

val tl_dyn :
  ?share:bool ->
  ('n, 'x_cd) vec -> ('n Slap_size.p, 'x_cd) vec
(** @return a given vector without the first element.
    @param share [true] if a returned subvector refers a given vector
           (default = [false])
    @since 4.0.0 *)

val inits :
  ?share:bool ->
  ('n Slap_size.s, 'x_cd) vec -> ('n, 'x_cd) vec
(** @return a given vector without the first element. (typesafe)
    This is the same as {i init} in Haskell.
    @param share [true] if a returned subvector refers a given vector
           (default = [false])
    @since 4.0.0 *)

val inits_dyn :
  ?share:bool ->
  ('n, 'x_cd) vec -> ('n Slap_size.p, 'x_cd) vec
(** @return a given vector without the first element.
    This is the same as {i init} in Haskell.
    @param share [true] if a returned subvector refers a given vector
           (default = [false])
    @since 4.0.0 *)

val copy : ?y:('n, 'y_cd) vec -> ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [copy ?y x] copies the vector [x] to the vector [y] with the BLAS-1
    function [[sdcz]copy].
    @return the vector [y], which is overwritten.
    @param y default = a fresh vector.
 *)

val fill : ('n, 'cd) vec -> num_type -> unit
(** Fill the given vector with the given value. *)

val append :
  ('m, 'x_cd) vec -> ('n, 'y_cd) vec ->
  (('m, 'n) Slap_size.add, 'cnt) vec
(** Concatenate two vectors. *)

val shared_rev : ('n, 'cd) vec -> ('n, Slap_misc.dsc) vec
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

val of_array_dyn :
  'n Slap_size.t ->
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

val unsafe_of_array :
  'n Slap_size.t -> num_type array -> ('n, 'cnt) vec
(** Like [of_array_dyn], but size checking is not always performed.
    @since 1.0.0 *)

val to_list : ('n, 'cd) vec -> num_type list
(** [to_list x]
    @return the list of all the elements of the vector [x].
 *)

val of_list_dyn :
  'n Slap_size.t ->
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

val unsafe_of_list :
  'n Slap_size.t -> num_type list -> ('n, 'cnt) vec
(** Like [of_list_dyn], but size checking is not always performed.
    @since 1.0.0 *)

val to_bigarray :
  ('n, 'cd) vec ->
  (num_type, prec, fortran_layout) Array1.t
(** [to_bigarray x]
    @return the big array of all the elements of the vector [x].
 *)

val of_bigarray_dyn :
  ?share:bool ->
  'n Slap_size.t ->
  (num_type, prec, fortran_layout) Array1.t ->
  ('n, 'cnt) vec
(** [of_bigarray_dyn ?share n ba]
    @raise Invalid_argument the length of the given big array is not equal to
          [n].
    @return a fresh vector of all the elements of big array [ba].
    @param share [true] if data are shared. (default = [false])
 *)

val of_bigarray :
  ?share:bool ->
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

val unsafe_of_bigarray :
  ?share:bool ->
  'n Slap_size.t ->
  (num_type, prec, fortran_layout) Array1.t -> ('n, 'cnt) vec
(** Like [of_bigarray_dyn], but size checking is not always performed.
    @since 1.0.0 *)


#if OCAML_MAJOR >= 4

val of_array_c :
  num_type array -> (num_type, prec, 'cnt) Slap_vec.dyn
(** [let Slap.Vec.VEC n = of_array_c [|a1; ...; an|]]
    @return a constructor [VEC] that has a vector [(a1, ..., an)] that has the
    existential quantified sized type
    [exists n. (n, 'num, 'prec, 'cnt_or_dsc) Vec.t].
    "c" of [of_array_c] means a "c"onstructor of GADT. This function is
    available in OCaml 4.00 or above.
    @since 4.0.0 *)

val of_list_c :
  num_type list -> (num_type, prec, 'cnt) Slap_vec.dyn
(** [let Slap.Vec.VEC n = of_list_c [a1; ...; an]]
    @return a constructor [VEC] that has a vector [(a1, ..., an)] that has the
    existential quantified sized type
    [exists n. (n, 'num, 'prec, 'cnt_or_dsc) Vec.t].
    "c" of [of_list_c] means a "c"onstructor of GADT. This function is available
    in OCaml 4.00 or above.
    @since 4.0.0 *)

val of_bigarray_c :
  ?share:bool ->
  (num_type, prec, fortran_layout) Array1.t ->
  (num_type, prec, 'cnt) Slap_vec.dyn
(** [let Slap.Vec.VEC n = of_bigarray_c ?share ba]
    @return a constructor [VEC] that has a vector (of all the elements of big
    array [ba]) that has the existential quantified sized type
    [exists n. (n, 'num, 'prec, 'cnt_or_dsc) Vec.t].
    "c" of [of_bigarray_c] means a "c"onstructor of GADT. This function is
    available in OCaml 4.00 or above.
    @param share [true] if data are shared. (default = [false])
    @since 4.0.0 *)

#endif


(** {2 Iterators} *)

val map :
  (num_type -> num_type) ->
  ?y:('n, 'y_cd) vec ->
  ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [map f ?y (x1, ..., xn)] is [(f x1, ..., f xn)].
   @return the vector [y], which is overwritten.
   @param y default = a fresh vector.
 *)

val mapi :
  (int -> num_type -> num_type) ->
  ?y:('n, 'y_cd) vec ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec
(** [mapi f ?y (x1, ..., xn)] is [(f 1 x1, ..., f n xn)] with
   the vector's dimension [n].
   @return the vector [y], which is overwritten.
   @param y default = a fresh vector.
 *)

val fold_left :
  ('accum -> num_type -> 'accum) ->
  'accum ->
  ('n, 'cd) vec -> 'accum
(** [fold_left f init (x1, x2, ..., xn)] is
   [f (... (f (f init x1) x2) ...) xn].
 *)

val fold_lefti :
  (int -> 'accum -> num_type -> 'accum) ->
  'accum ->
  ('n, 'cd) vec -> 'accum
(** [fold_lefti f init (x1, x2, ..., xn)] is
   [f n (... (f 2 (f 1 init x1) x2) ...) xn] with the vector's dimension [n].
 *)

val fold_right :
  (num_type -> 'accum -> 'accum) ->
  ('n, 'cd) vec ->
  'accum -> 'accum
(** [fold_right f (x1, x2, ..., xn) init] is
   [f x1 (f x2 (... (f xn init) ...))].
 *)

val fold_righti :
  (int -> num_type -> 'accum -> 'accum) ->
  ('n, 'cd) vec ->
  'accum -> 'accum
(** [fold_righti f (x1, x2, ..., xn) init] is
   [f 1 x1 (f 2 x2 (... (f n xn init) ...))] with the vector's dimension [n].
 *)

val replace_all : ('n, 'cd) vec -> (num_type -> num_type) -> unit
(** [replace_all x f] modifies the vector [x] in place
   -- the [i]-th element [xi] of [x] will be set to [f xi].
 *)

val replace_alli : ('n, 'cd) vec -> (int -> num_type -> num_type) -> unit
(** [replace_alli x f] modifies the vector [x] in place
   -- the [i]-th element [xi] of [x] will be set to [f i xi].
 *)

val iter : (num_type -> unit) -> ('n, 'cd) vec -> unit
(** [iter f (x1, x2, ..., xn)] is [f x1; f x2; ...; f xn].
 *)

val iteri : (int -> num_type -> unit) -> ('n, 'cd) vec -> unit
(** [iteri f (x1, x2, ..., xn)] is [f 1 x1; f 2 x2; ...; f n xn].
 *)

(** {2 Iterators on two vectors} *)

val map2 :
  (num_type -> num_type -> num_type) ->
  ?z:('n, 'z_cd) vec ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec ->
  ('n, 'z_cd) vec
(** [map2 f ?z (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(f x1 y1, f x2 y2, ..., f xn yn)].
    @return the vector [z], which is overwritten.
    @param z default = a fresh vector.
 *)

val mapi2 :
  (int -> num_type -> num_type -> num_type) ->
  ?z:('n, 'z_cd) vec ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec ->
  ('n, 'z_cd) vec
(** [mapi2 f ?z (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(f 1 x1 y1, f 2 x2 y2, ..., f n xn yn)] with the vectors' dimension [n].
    @return the vector [z], which is overwritten.
    @param z default = a fresh vector.
 *)

val fold_left2 :
  ('accum -> num_type -> num_type -> 'accum) ->
  'accum ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> 'accum
(** [fold_left2 f init (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f (... (f (f init x1 y1) x2 y2) ...) xn yn].
 *)

val fold_lefti2 :
  (int -> 'accum -> num_type -> num_type -> 'accum) ->
  'accum ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> 'accum
(** [fold_lefti2 f init (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f n (... (f 2 (f 1 init x1 y1) x2 y2) ...) xn yn] with the vectors'
    dimension [n].
 *)

val fold_right2 :
  (num_type -> num_type -> 'accum -> 'accum) ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec ->
  'accum -> 'accum
(** [fold_righti2 f (x1, x2, ..., xn) (y1, y2, ..., yn) init] is
    [f x1 y1 (f x2 y2 (... (f xn yn init) ...))].
 *)

val fold_righti2 :
  (int -> num_type -> num_type -> 'accum -> 'accum) ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec ->
  'accum -> 'accum
(** [fold_righti2 f (x1, x2, ..., xn) (y1, y2, ..., yn) init] is
    [f 1 x1 y1 (f 2 x2 y2 (... (f n xn yn init) ...))] with the vectors'
    dimension [n].
 *)

val iter2 :
  (num_type -> num_type -> unit) ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> unit
(** [iter2 f (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f x1 y1; f x2 y2; ...; f xn yn].
 *)

val iteri2 :
  (int -> num_type -> num_type -> unit) ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> unit
(** [iteri2 f (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [f 1 x1 y1; f 2 x2 y2; ...; f n xn yn].
 *)

(** {2 Iterators on three vectors} *)

val map3 :
  (num_type -> num_type -> num_type -> num_type) ->
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

val mapi3 :
  (int -> num_type -> num_type -> num_type -> num_type) ->
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

val fold_left3 :
  ('accum -> num_type -> num_type -> num_type -> 'accum) ->
  'accum ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec ->
  ('n, 'z_cd) vec -> 'accum
(** [fold_left3 f init (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)]
    is [f (... (f (f init x1 y1 z1) x2 y2 z2) ...) xn yn zn].
 *)

val fold_lefti3 :
  (int -> 'accum -> num_type -> num_type -> num_type->'accum) ->
  'accum ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec ->
  ('n, 'z_cd) vec -> 'accum
(** [fold_lefti3 f init (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)]
    is [f n (... (f 2 (f 1 init x1 y1 z1) x2 y2 z2) ...) xn yn zn] with the
    vectors' dimension [n].
 *)

val fold_right3 :
  (num_type -> num_type -> num_type -> 'accum -> 'accum) ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec ->
  ('n, 'z_cd) vec ->
  'accum -> 'accum
(** [fold_right3 f (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn) init]
    is [f x1 y1 z1 (f x2 y2 z2 (... (f xn yn zn init) ...))].
 *)

val fold_righti3 :
  (int -> num_type -> num_type -> num_type -> 'accum->'accum)->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec ->
  ('n, 'z_cd) vec ->
  'accum -> 'accum
(** [fold_righti3 f (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn) init]
    is [f 1 x1 y1 z1 (f 2 x2 y2 z2 (... (f n xn yn zn init) ...))]
    with the vectors' dimension [n].
 *)

val iter3 :
  (num_type -> num_type -> num_type -> unit) ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec ->
  ('n, 'z_cd) vec -> unit
(** [iter3 f (x1, x2, ..., xn) (y1, y2, ..., yn) (z1, z2, ..., zn)] is
    [f x1 y1 z1; f x2 y2 z2; ...; f xn yn zn].
 *)

val iteri3 :
  (int -> num_type -> num_type -> num_type -> unit) ->
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

val for_all2 :
  (num_type -> num_type -> bool) ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> bool
(** [for_all2 p (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(p x1 y1) && (p x2 y2) && ... && (p xn yn)].
    @return [true] if and only if all elements of the given two vectors
    satisfy the predicate [p].
 *)

val exists2 :
  (num_type -> num_type -> bool) ->
  ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> bool
(** [exists2 p (x1, x2, ..., xn) (y1, y2, ..., yn)] is
    [(p x1 y1) || (p x2 y2) || ... || (p xn yn)].
    @return [true] if and only if at least one pair of elements of the given two
    vectors satisfies the predicate [p].
 *)

(** {2 Arithmetic operations} *)

val max : ('n, 'cd) vec -> num_type
(** [max x]
    @return the largest element in [x]. [NaN]s is ignored. If only [NaN]s are
            encountered, the negative [infinity] value is returned.
 *)

val min : ('n, 'cd) vec -> num_type
(** [min x]
    @return the smallest element in [x]. [NaN]s is ignored. If only [NaN]s are
            encountered, the positive [infinity] value is returned.
 *)

val sum : ('n, 'cd) vec -> num_type
(** [sum x]
    @return the sum of all elements in [x].
 *)

val prod : ('n, 'cd) vec -> num_type
(** [prod x]
    @return the product of all elements in [x].
 *)

val add_const :
  num_type ->
  ?y:('n, 'y_cd) vec ->
  ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [add_const c ?y x] adds constant value [c] to all elements in vector [x].
    @return the vector [y], which is overwritten.
    @since 0.1.0
 *)

val sqr_nrm2 : ?stable:bool -> ('n, 'cd) vec -> float
(** [sqr_nrm2 ?stable x] computes the square of the 2-norm (Euclidean norm) of
    vector [x].

    @param stable default = [false].
      - If [stable] = [true], BLAS function [nrm2] is used;
      - If [stable] = [false], BLAS function [dot] is used instead for greatly
        improved performance.
 *)

val ssqr : ?c:num_type -> ('n, 'cd) vec -> num_type
(** [ssqr ?c (x1, x2, ..., xn)] computes the sum of squared differences of the
    elements in the given vector from scalar value [c]:
    [(x1 - c)^2 + (x2 - c)^2 + ... + (xn - c)^2].

    @param c default = [0.0].
 *)

val sort :
  ?cmp:(num_type -> num_type -> int) ->
  ?decr:bool ->
  ?p:('n, 'p_cd) int_vec ->
  ('n, 'x_cd) vec -> unit
(** [sort ?cmp ?decr ?p x] sorts the elements in vector [x] in increasing order
    according to the comparison function [cmp].

    @param cmp default = the usual order (on real numbers) or the lexicographic
               order (on complex numbers).
      - [cmp a b < 0] iff [a < b];
      - [cmp a b = 0] iff [a = b];
      - [cmp a b > 0] iff [a > b].
      - [NaN]s must be considered larger than any other value.
    @param decr sort in decreasing order.
    @param p    If [p] is passed, [x] is unchanged and the permutation is stored
                into [p]. (default = [None].)
 *)

val neg : ?y:('n, 'y_cd) vec -> ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [neg ?y (x1, x2, ..., xn)] returns [(-x1, -x2, ..., -xn)].
    @return the vector [y], which is overwritten.
 *)

val reci :
  ?y:('n, 'y_cd) vec ->
  ('n, 'x_cd) vec -> ('n, 'y_cd) vec
(** [reci ?y (x1, x2, ..., xn)] returns [(1 / x1, 1 / x2, ..., 1 / xn)].
    @return the vector [y], which is overwritten.
    @since 0.1.0
 *)

val add :
  ?z:('n, 'z_cd) vec -> ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> ('n, 'z_cd) vec
(** [add ?z (x1, x2, ..., xn) (y1, y2, ..., yn)] returns
    [(x1 + y1, x2 + y2, ..., xn + yn)].
    @return the vector [z], which is overwritten.
 *)

val sub :
  ?z:('n, 'z_cd) vec -> ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> ('n, 'z_cd) vec
(** [sub ?z (x1, x2, ..., xn) (y1, y2, ..., yn)] returns
    [(x1 - y1, x2 - y2, ..., xn - yn)].
    @return the vector [z], which is overwritten.
 *)

val mul :
  ?z:('n, 'z_cd) vec -> ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> ('n, 'z_cd) vec
(** [mul ?z (x1, x2, ..., xn) (y1, y2, ..., yn)] returns
    [(x1 * y1, x2 * y2, ..., xn * yn)].
    @return the vector [z], which is overwritten.
 *)

val div :
  ?z:('n, 'z_cd) vec -> ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> ('n, 'z_cd) vec
(** [div ?z (x1, x2, ..., xn) (y1, y2, ..., yn)] returns
    [(x1 / y1, x2 / y2, ..., xn / yn)].
    @return the vector [z], which is overwritten.
 *)

val zpxy :
  ('n, 'z_cd) vec -> ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> ('n, 'z_cd) vec
(** [zpxy (z1, z2, ..., zn) (x1, x2, ..., xn) (y1, y2, ..., yn)]
    returns [(z1 + x1 * y1, z2 + x2 * y2, ..., zn + xn * yn)].
    This function is useful for convolutions.
    @return the vector [z], which is overwritten.
    @since 0.1.0 *)

val zmxy :
  ('n, 'z_cd) vec -> ('n, 'x_cd) vec ->
  ('n, 'y_cd) vec -> ('n, 'z_cd) vec
(** [zmxy (z1, z2, ..., zn) (x1, x2, ..., xn) (y1, y2, ..., yn)]
    returns [(z1 - x1 * y1, z2 - x2 * y2, ..., zn - xn * yn)].
    This function is useful for convolutions.
    @return the vector [z], which is overwritten.
    @since 0.1.0 *)

val ssqr_diff : ('n, 'x_cd) vec -> ('n, 'y_cd) vec -> num_type
(** [ssqr_diff x y]returns the sum of squared differences of the elements of
    vectors [x] and [y].
 *)

(** {2 Subvectors} *)

val subcntvec_dyn :
  'm Slap_size.t ->
  ?ofsx:int ->
  ('n, cnt) vec ->
  ('m, 'cnt) vec
(** [subcntvec_dyn m ?ofsx x]
    @return a subvector of the contiguous vector [x].
    The [i]-th element of it refers [(ofsx+i-1)]-th element of [x].
    The data are shared.
    @param ofsx default = 1
    @raise Invalid_argument [m] and [ofsx] do not designate a valid subvector of
    [x].
 *)

val subdscvec_dyn :
  'm Slap_size.t ->
  ?ofsx:int ->
  ?incx:int ->
  ('n, 'cd) vec ->
  ('m, dsc) vec
(** [subdscvec_dyn m ?ofsx ?incx x]
    @return a subvector of the vector [x].
    The [i]-th element of it refers [(ofsx+(i-1)*incx)]-th element of [x].
    The data are shared.
    @param ofs default = 1
    @param inc default = 1
    @raise Invalid_argument [m], [ofsx] and [incx] do not designate
    a valid subvector of [x].
 *)

val subvec_dyn :
  'm Slap_size.t ->
  ?ofsx:int ->
  ?incx:int ->
  ('n, 'cd) vec ->
  ('m, dsc) vec
(** An alias of [subdscvec_dyn]. *)
