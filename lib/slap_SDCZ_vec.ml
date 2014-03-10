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

(** A part of the signature of [Slap.[SDCZ].Vec]. *)

module type S =
sig
  (* implementation: slap_SDCZ_vec_wrap.ml *)

  include Slap_SDCZ_types.S

  (** The type of modules including a generative phantom type and
   a dynamically-sized vector.
   *)
  module type VEC =
    sig
      type n
      val value : (n, 'cnt) vec
    end

  (** {2 Creation of vectors} *)

  val create : 'n Common.size -> ('n, 'cnt) vec
  (** [create n]
   @return a fresh [n]-dimensional vector (not initialized).
   *)

  val make : 'n Common.size -> num_type -> ('n, 'cnt) vec
  (** [make n a]
   @return a fresh [n]-dimensional vector initialized with [a].
   *)

  val make0 : 'n Common.size -> ('n, 'cnt) vec
  (** [make0] is an alias of [zeros]. *)

  val zeros : 'n Common.size -> ('n, 'cnt) vec
  (** [zeros n]
   @return a fresh [n]-dimensional vector initialized with [0].
   *)

  val ones : 'n Common.size -> ('n, 'cnt) vec
  (** [ones n]
   @return a fresh [n]-dimensional vector initialized with [1].
   *)

  val init : 'n Common.size ->
             f:(int -> num_type) -> ('n, 'cnt) vec
  (** [init n ~f]
   @return a fresh vector [(f 1, ..., f n)] with [n] elements.
   *)

  (** {2 Accessors} *)

  val dim : ('n, 'cd) vec -> 'n Common.size
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
    f:(num_type -> num_type) -> unit
  (** [replace_dyn v i ~f] is [set_dyn v i (f (get_dyn v i))]. *)

  (** {2 Basic operations} *)

  val copy : ?y:('n, 'y_cd) vec -> ('n, 'x_cd) vec -> ('n, 'y_cd) vec
  (** [copy ?y x] copies the vector [x] to the vector [y] with the BLAS-1
   function [[sdcz]copy].
   @return the vector [y], which is overwritten.
   @param y default = a fresh vector.
   *)

  val fill : ('n, 'cd) vec -> num_type -> unit
  (** Fill the given vector with the given value. *)

  (** {2 Type conversion} *)

  val to_array : ('n, 'cd) vec -> num_type array
  (** [to_array x]
   @return the array of all the elements of the vector [x].
   *)

  val of_array_dyn : 'n Common.size ->
                     num_type array -> ('n, 'cnt) vec
  (** [of_array_dyn n [|a1; ...; an|]]
   @raise Invalid_argument the length of the given array is not equal to [n].
   @return a fresh vector [(a1, ..., an)].
   *)

  module Of_array (X : sig val value : num_type array end) : VEC
  (** [Of_array(struct let value = [|a1; ...; an|] end)] returns a fresh module
    including the dynamically-sized vector [(a1, ..., an)].
   *)

  val to_list : ('n, 'cd) vec -> num_type list
  (** [to_list x]
   @return the list of all the elements of the vector [x].
   *)

  val of_list_dyn : 'n Common.size ->
                     num_type list -> ('n, 'cnt) vec
  (** [of_list_dyn n [a1; ...; an]]
   @raise Invalid_argument the length of the given list is not equal to [n].
   @return a fresh vector [(a1, ..., an)].
   *)

  module Of_list (X : sig val value : num_type list end) : VEC
  (** [Of_list(struct let value = [a1; ...; an] end)] returns a fresh module
    including the dynamically-sized vector [(a1, ..., an)].
   *)

  (** {2 Iterators} *)

  val map : ?y:('n, 'y_cd) vec ->
            f:(num_type -> num_type) ->
            ('n, 'x_cd) vec -> ('n, 'y_cd) vec
  (** [map ?y ~f (x1, ..., xn)] is [(f x1, ..., f xn)].
   @return the vector [y], which is overwritten.
   @param y default = a fresh vector.
   *)

  val mapi : ?y:('n, 'y_cd) vec ->
             f:(int -> num_type -> num_type) ->
             ('n, 'x_cd) vec ->
             ('n, 'y_cd) vec
  (** [mapi ?y ~f (x1, ..., xn)] is [(f 1 x1, ..., f n xn)] with
   the vector's dimension [n].
   @return the vector [y], which is overwritten.
   @param y default = a fresh vector.
   *)

  val fold_left : f:('accum -> num_type -> 'accum) ->
                  init:'accum ->
                  ('n, 'cd) vec -> 'accum
  (** [fold_left ~f ~init (x1, x2, ..., xn)] is
   [f (... (f (f init x1) x2) ...) xn].
   *)

  val fold_lefti : f:(int -> 'accum -> num_type -> 'accum) ->
                   init:'accum ->
                   ('n, 'cd) vec -> 'accum
  (** [fold_lefti ~f ~init (x1, x2, ..., xn)] is
   [f n (... (f 2 (f 1 init x1) x2) ...) xn] with the vector's dimension [n].
   *)

  val fold_right : f:(num_type -> 'accum -> 'accum) ->
                   ('n, 'cd) vec ->
                   init:'accum -> 'accum
  (** [fold_right ~f (x1, x2, ..., xn) ~init] is
   [f x1 (f x2 (... (f xn init) ...))].
   *)

  val fold_righti : f:(int -> num_type -> 'accum -> 'accum) ->
                    ('n, 'cd) vec ->
                    init:'accum -> 'accum
  (** [fold_righti ~f (x1, x2, ..., xn) ~init] is
   [f 1 x1 (f 2 x2 (... (f n xn init) ...))] with the vector's dimension [n].
   *)

  val replace_all : ('n, 'cd) vec ->
    f:(num_type -> num_type) -> unit
  (** [replace_all x ~f] modifies the vector [x] in place
   -- the [i]-th element [xi] of [x] will be set to [f xi].
   *)

  val replace_alli : ('n, 'cd) vec ->
    f:(int -> num_type -> num_type) -> unit
  (** [replace_alli x ~f] modifies the vector [x] in place
   -- the [i]-th element [xi] of [x] will be set to [f i xi].
   *)

  val iter : f:(num_type -> unit) ->
             ('n, 'cd) vec -> unit
  (** [iter ~f (x1, x2, ..., xn)] is [f x1; f x2; ...; f xn].
  *)

  val iteri : f:(int -> num_type -> unit) ->
              ('n, 'cd) vec -> unit
  (** [iteri ~f (x1, x2, ..., xn)] is [f 1 x1; f 2 x2; ...; f n xn].
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
end
