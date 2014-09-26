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

type +'n t = private int
(** A singleton type on sizes (i.e., dimensions of vectors and matrices).

    Evaluation of a term with {i singleton type} ['n Size.t] {b always} results
    in the natural number corresponding to phantom type parameter ['n].
    ['n] is instantiated to a generative phantom type or a (phantom) type that
    represents an arithmetic operation defined in this module. In either case,
    {b only} the equality of sizes is verified statically.
 *)

(** {2 Constants} *)

type z
(** zero *)

type 'n s
(** successor, i.e., ['n s] corresponds to ['n + 1]. *)

val zero : z t

val one : z s t

val two : z s s t

val three : z s s s t

val four : z s s s s t

val five : z s s s s s t

type ten = z s s s s s s s s s s

val ten : ten t

(** {2 Arithmetic operations} *)

val succ : 'n t -> 'n s t
(** [succ n]
    @return [n] + [1]
 *)

type 'n p

val pred_dyn : 'n t -> 'n p t
(** [pred_dyn n]
    @return [n] - [1]
    @raise Invalid_arg if [n <= 0].
    @since 0.2.0
 *)

type ('m, 'n) add

val add : 'm t -> 'n t -> ('m, 'n) add t
(** [add m n]
    @return [m] + [n]
 *)

type ('m, 'n) sub

val sub_dyn : 'm t -> 'n t -> ('m, 'n) sub t
(** [sub_dyn m n]
    @return [m] - [n]
    @raise Invalid_argument [m] < [n]
 *)

type ('m, 'n) mul

val mul : 'm t -> 'n t -> ('m, 'n) mul t
(** [mul m n]
    @return the product of [m] and [n]
 *)

type ('m, 'n) div

val div_dyn : 'm t -> 'n t -> ('m, 'n) div t
(** [div_dyn m n]
    @return [m] / [n]
    @raise Invalid_argument [n] is zero
 *)

type ('m, 'n) min

val min : 'm t -> 'n t -> ('m, 'n) min t
(** [min m n]
    @return the minimum of [m] and [n]
 *)

type ('m, 'n) max

val max : 'm t -> 'n t -> ('m, 'n) max t
(** [max m n]
    @return the maximum of [m] and [n]
 *)

(** {2 Storage sizes for BLAS and LAPACK} *)

type 'n packed
(** Type ['n packed] corresponds to the {i packed storage} size of a [n]-by-[n]
    matrix.
    @see <http://www.netlib.org/lapack/lug/node123.html>
         Packed Storage (BLAS & LAPACK)
    @since 0.2.0
 *)

val packed : 'n t -> 'n packed t
(** [packed n] computes the packed storage size of a [n]-by-[n] matrix.
    @since 0.2.0
 *)

val unpacked : 'n packed t -> 'n t
(** [unpacked n] computes the inverse of the packed storage size, i.e.,
    [unpacked (packed n) = n] for all [n].
    @since 0.2.0
 *)

type ('m, 'n, 'kl, 'ku) geband
(** [('m, 'n, 'kl, 'ku) geband] represents {i band storage} size:
    A ['m]-by-['n] band matrix with ['kl] subdiagonals and ['ku] superdiagonals
    is stored in a [('kl+'ku+1)]-by-['n] matrix where ['kl, 'ku << min('m, 'n)].
    [('m, 'kl, 'ku) geband] corresponds to ['kl + 'ku + 1].

    @see <http://www.netlib.org/lapack/lug/node124.html>
         Band Storage (BLAS & LAPACK)
    @since 0.2.0
*)

val geband_dyn : 'm t -> 'n t -> 'kl t -> 'ku t -> ('m, 'n, 'kl, 'ku) geband t
(** [geband_dyn m n kl ku] computs the band storage size of [m]-by-[n] band
    matrices with [kl] subdiagonals and [ku] superdiagonals.

    @param m the number of rows
    @param n the number of rows
    @param kl the number of subdiagonals
    @param ku the number of superdiagonals

    @raise Invalid_argument if [m >= kl] and [n >= ku].

    @since 0.2.0
 *)

type ('n, 'kd) syband
(** [('n, 'kd) syband] represents {i symmetric or Hermitian band storage} size:
    A [n]-by-[n] symmetric or Hermitian band matrix with [kd] superdiagonals or
    subdiagonals is stored in a [(kd+1)]-by-[n] matrix where [kd << n].
    [('n, 'kd) syband] corresponds to [kd + 1].

    @see <http://www.netlib.org/lapack/lug/node124.html>
         Band Storage (BLAS & LAPACK)
    @since 0.2.0
*)

val syband_dyn : 'n t -> 'kd t -> ('n, 'kd) syband t
(** [syband_dyn n kd] computs the band storage size of symmetric or Hermitian
    band matrices with [kd] superdiagonals or subdiagonals.

    @param n the number of rows or columns
    @param kd the number of superdiagonals or subdiagonals

    @raise Invalid_argument if [kd >= n].

    @since 0.2.0
 *)

type ('m, 'n, 'kl, 'ku) luband = ('m, 'n, 'kl, ('kl, 'ku) add) geband
(** [('m, 'n, 'kl, 'ku) luband] represents {i band storage} size for LU
    factorization: A ['m]-by-['n] band matrix with ['kl] subdiagonals and ['ku]
    superdiagonals for LU factorization is stored in band storage format with
    ['kl+'ku] superdiagonals.

    @see <http://www.netlib.org/lapack/lug/node124.html>
         Band Storage (BLAS & LAPACK)
    @since 0.2.0
*)

val luband_dyn : 'm t -> 'n t -> 'kl t -> 'ku t -> ('m, 'n, 'kl, 'ku) luband t
(** [luband_dyn m n kl ku] computs the band storage size for LU factorization of
    [m]-by-[n] band matrices with [kl] subdiagonals and [ku] superdiagonals.

    @param m the number of rows
    @param n the number of columns
    @param kl the number of subdiagonals
    @param ku the number of superdiagonals

    @raise Invalid_argument if [m >= kl] and [n >= ku].

    @since 0.2.0
 *)

(** {2 Conversion between a size and an integer} *)

(** The signature of modules as packages of types like [exists n. n Size.t]. *)
module type SIZE =
  sig
    type n
    (** A generative phantom type. *)

    val value : n t
    (** A dynamically-decided size with type like
        [exists n. n Size.t]. *)
  end

val to_int : 'n t -> int
(** Return the integer correponding to the given size. *)

val of_int_dyn : int -> (module SIZE)
(** [module N = (val of_int_dyn n : SIZE)]
    @return module [N] containing the size [N.value] (= [n]) that has the type
    [N.n Size.t] with a generative phantom type [N.n] as a package of an
    existential quantified sized type like [exists n. n Size.t].
    @raise Invalid_argument the given size is negative.
 *)

val unsafe_of_int : int -> (module SIZE)
(** Like [of_int_dyn], but dynamic size checking is not performed. *)

module Of_int_dyn :
functor (N : sig val value : int end) -> SIZE
(** A functor version of [of_int_dyn]. *)

(** {2 Iterators on sizes}

    The following functions are iterators over [[1; 2; ...; n]] where [n] is a
    size.
 *)

val fold_left : ('accum -> (module SIZE) -> 'accum) ->
                'accum ->
                'n t -> 'accum
(** [fold_left f init n] is [f (... (f (f init 1) 2) ...) n]. *)

val fold_right : ((module SIZE) -> 'accum -> 'accum) ->
                 'n t ->
                 'accum -> 'accum
(** [fold_right f n init] is [f 1 (f 2 (... (f n init) ...))]. *)

val iter : ((module SIZE) -> unit) -> 'n t -> unit
(** [iter f n] is [f 1; f 2; ...; f n]. *)

val riter : ((module SIZE) -> unit) -> 'n t -> unit
(** [riter f n] is [f n; ...; f 2; f 1]. *)

(** {2 Iterators on integers}

    The following functions are iterators over [[1; 2; ...; to_int n]] where
    [n] is a size.
 *)

val fold_lefti : ('accum -> int -> 'accum) ->
                 'accum ->
                 'n t -> 'accum
(** [fold_lefti f init n] is [f (... (f (f init 1) 2) ...) (to_int n)]. *)

val fold_righti : (int -> 'accum -> 'accum) ->
                  'n t ->
                  'accum -> 'accum
(** [fold_righti f n init] is [f 1 (f 2 (... (f (to_int n) init) ...))]. *)

val iteri : (int -> unit) -> 'n t -> unit
(** [iteri f n] is [f 1; f 2; ...; f (to_int n)]. *)

val riteri : (int -> unit) -> 'n t -> unit
(** [riteri f n] is [f (to_int n); ...; f 2; f 1]. *)
