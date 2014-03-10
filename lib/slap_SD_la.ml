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

(** A part of the signature of [Slap.[SD]]. *)

module type S =
sig
  (* implementation: slap_SD_la_wrap.ml *)

  include Slap_SDCZ_la.S

  (** {2 BLAS interface} *)

  (** {3 Level 1} *)

  val dot : x:('n, 'x_cd) vec -> ('n, 'y_cd) vec -> float
  (** [dot ~x y]
   @return the inner product of the vector [x] and [y].
   *)

  val asum : ('n, 'x_cd) vec -> float
  (** [asum x]
   @return the sum of absolute values of elements in the vector [x].
   *)

  (** {3 Level 2} *)

  val ger : ?alpha:float -> ('m, 'x_cd) vec -> ('n, 'y_cd) vec ->
            ('m, 'n, 'a_cd) mat -> ('m, 'n, 'a_cd) mat
  (** [ger ?alpha x y a] computes [a := alpha * x * y^T + a] with
   the general matrix [a], the vector [x] and
   the transposed vector [y^T] of [y].
   @return [a], which is overwritten.
   @param alpha default = [1.0]
   *)

  val syr : ?alpha:float -> ?up:bool -> ('n, 'x_cd) vec ->
            ('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat
  (** [syr ?alpha x a] computes [a := alpha * x * x^T + a] with
   the symmetric matrix [a], the vector [x] and
   the transposed vector [x^T] of [x].
   @return [a], which is overwritten.
   @param alpha default = [1.0]
   @param up    default = [true], i.e., the upper triangular part of [a] is
                supplied.
 *)

  (** {2 LAPACK interface} *)

  (** {3 Linear equations (computational routines)} *)

  val orgqr_dyn : tau:('k, Common.cnt) vec ->
                  ('m, 'n, 'cd) mat -> unit
  (** [orgqr_dyn ~tau a] generates the orthogonal matrix [Q] of the QR
   factorization formed by [geqrf]/[geqpf].

   The matrix [a] and the vector [tau] must satisfy the following inequality:
   [(Mat.dim1 a) >= (Mat.dim2 a) >= (Vec.dim tau)], i.e., ['m >= 'n >= 'k].
   *)

  (** {3 Symmetric-matrix eigenvalue and singular value problems
         (simple drivers)} *)

  val syev : ?vectors:bool -> ?up:bool -> ?w:('n, Common.cnt) vec ->
             ('n, 'n, 'cd) mat -> ('n, 'cnt) vec
end
