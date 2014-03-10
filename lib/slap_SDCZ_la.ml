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

(** A part of the signature of [Slap.[SDCZ]]. *)

module type S =
sig
  (* implementation: slap_SDCZ_la_wrap.ml *)

  include Slap_SDCZ_types.S

  val kind : (num_type, prec) Bigarray.kind

  val real_kind : (float, real_prec) Bigarray.kind

  val prec : (num_type, prec) Bigarray.kind
  (** An alias of [kind]. *)

  val pp_num : Format.formatter -> num_type -> unit
  (** A pretty-printer for elements in vectors and matrices. *)

  val pp_vec : Format.formatter -> ('n, Common.cnt) vec -> unit
  (** A pretty-printer for column vectors. *)

  val pp_mat : Format.formatter -> ('m, 'n, Common.cnt) mat -> unit
  (** A pretty-printer for matrices. *)

  (** {2 BLAS interface} *)

  (** {3 Level 1} *)

  val swap : x:('n, 'x_cd) vec -> ('n, 'y_cd) vec -> unit

  val scal : num_type -> ('n, 'cd) vec -> unit

  val copy : ?y:('n, 'y_cd) vec -> ('n, 'x_cd) vec -> ('n, 'y_cd) vec

  val nrm2 : ('n, 'cd) vec -> float

  val axpy : ?alpha:num_type -> x:('n, 'x_cd) vec -> ('n, 'y_cd) vec -> unit

  val iamax : ('n, 'cd) vec -> int

  val amax : ('n, 'cd) vec -> num_type

  (** {3 Level 2} *)

  val gemv : ?beta:num_type ->
             ?y:('m, 'y_cd) vec ->
             trans:(('a_m, 'a_n, 'a_cd) mat -> ('m, 'n, 'a_cd) mat) trans ->
             ?alpha:num_type ->
             ('a_m, 'a_n, 'a_cd) mat ->
             ('n, 'x_cd) vec -> ('m, 'y_cd) vec

  val symv : ?beta:num_type ->
             ?y:('n, 'y_cd) vec ->
             ?up:bool ->
             ?alpha:num_type ->
             ('n, 'n, 'a_cd) mat ->
             ('n, 'x_cd) vec -> ('n, 'y_cd) vec

  val trmv : trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans ->
             ?diag:Common.diag ->
             ?up:bool ->
             ('n, 'n, 'a_cd) mat ->
             ('n, 'x_cd) vec -> unit

  val trsv : trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans ->
             ?diag:Common.diag ->
             ?up:bool ->
             ('n, 'n, 'a_cd) mat ->
             ('n, 'x_cd) vec -> unit

  (** {3 Level 3} *)

  val gemm : ?beta:num_type ->
             ?c:('m, 'n, 'c_cd) mat ->
             transa:(('a_m, 'a_k, 'a_cd) mat -> ('m, 'k, 'a_cd) mat) trans ->
             ?alpha:num_type ->
             ('a_m, 'a_k, 'a_cd) mat ->
             transb:(('b_k, 'b_n, 'b_cd) mat -> ('k, 'n, 'b_cd) mat) trans ->
             ('b_k, 'b_n, 'b_cd) mat -> ('m, 'n, 'c_cd) mat

  val symm : side:('k, 'm, 'n) Common.side ->
             ?up:bool ->
             ?beta:num_type ->
             ?c:('m, 'n, 'c_cd) mat ->
             ?alpha:num_type ->
             ('k, 'k, 'a_cd) mat ->
             ('m, 'n, 'b_cd) mat -> ('m, 'n, 'c_cd) mat

  val trmm : side:('k, 'm, 'n) Common.side ->
             ?up:bool ->
             transa:(('k, 'k, 'a_cd) mat -> ('k, 'k, 'a_cd) mat) trans ->
             ?diag:Common.diag ->
             ?alpha:num_type ->
             a:('k, 'k, 'a_cd) mat ->
             ('m, 'n, 'b_cd) mat -> unit

  val trsm : side:('k, 'm, 'n) Common.side ->
             ?up:bool ->
             transa:(('k, 'k, 'a_cd) mat -> ('k, 'k, 'a_cd) mat) trans ->
             ?diag:Common.diag ->
             ?alpha:num_type ->
             a:('k, 'k, 'a_cd) mat ->
             ('m, 'n, 'b_cd) mat -> unit

  val syrk : ?up:bool ->
             ?beta:num_type ->
             ?c:('n, 'n, 'c_cd) mat ->
             trans:(('a_n, 'a_k, 'a_cd) mat ->
                    ('n, 'k, 'a_cd) mat) Common.trans2 ->
             ?alpha:num_type ->
             ('a_n, 'a_k, 'a_cd) mat -> ('n, 'n, 'c_cd) mat

  (** {2 LAPACK interface} *)

  (** {3 Auxiliary routines} *)

  val lacpy : ?uplo:[ `L | `U ] ->
              ?b:('m, 'n, 'b_cd) mat ->
              ('m, 'n, 'a_cd) mat -> ('m, 'n, 'b_cd) mat
  (** [lacpy ?uplo ?b a] copies the matrix [a] into the matrix [b].
   - If [uplo] is omitted, all elements in [a] is copied.
   - If [uplo] is [`U], the upper trapezoidal part of [a] is copied.
   - If [uplo] is [`L], the lower trapezoidal part of [a] is copied.
   @return [b], which is overwritten.
   @param uplo default = all elements in [a] is copied.
   @param b    default = a fresh matrix.
   *)

  val lange : ?norm:'a Common.norm4 ->
              ('m, 'n, 'cd) mat -> float
  (** [lange ?norm a]
   @return the norm of the matrix [a].
   @param norm default = [Slap.Common.norm_1]
   *)

  (** {3 Linear equations (computational routines)} *)

  val getrf : ?ipiv:(('m, 'n) Common.min, Common.cnt) Common.int32_vec ->
              ('m, 'n, 'cd) mat ->
              (('m, 'n) Common.min, Common.cnt) Common.int32_vec

  val potrf : ?up:bool ->
              ?jitter:num_type ->
              ('n, 'n, 'cd) mat -> unit

  val potri : ?up:bool ->
              ?factorize:bool ->
              ?jitter:num_type ->
              ('n, 'n, 'cd) mat -> unit

  val trtrs : ?up:bool ->
              trans:(('n, 'n, 'a_cd) mat -> ('n, 'n, 'a_cd) mat) trans ->
              ?diag:Common.diag ->
              ('n, 'n, 'a_cd) mat ->
              ('n, 'nrhs, 'b_cd) mat -> unit

  val geqrf : ?tau:(('m, 'n) Common.min, Common.cnt) vec ->
              ('m, 'n, 'cd) mat -> (('m, 'n) Common.min, 'cnt) vec

end
