(* Sized Linear Algebra Package (SLAP)

  Copyright (C) 2013- Akinori ABE <abe@kb.ecei.tohoku.ac.jp>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

module type TYPES =
sig
  type prec
  type num_type
  type vec = (num_type, prec, Bigarray.fortran_layout) Bigarray.Array1.t
  type rvec
  type mat = (num_type, prec, Bigarray.fortran_layout) Bigarray.Array2.t
  type trans3
end

(** Signatures of [Lacaml.[SDCZ]]. *)
module type SDCZ =
sig
  include TYPES

  val prec : (num_type, prec) Bigarray.kind

  module Vec :
  sig
    val create : int -> vec
    val make : int -> num_type -> vec
    val make0 : int -> vec
    val init : int -> (int -> num_type) -> vec
    val of_array : num_type array -> vec
    val to_array : vec -> num_type array
    val of_list : num_type list -> vec
    val to_list : vec -> num_type list
    val append : vec -> vec -> vec
    val concat : vec list -> vec
    val empty : vec
    val linspace :
      ?y:vec ->
      num_type -> num_type -> int -> vec
    val logspace :
      ?y:vec ->
      num_type ->
      num_type -> ?base:float -> int -> vec
    val dim : vec -> int
    val map :
      (num_type -> num_type) ->
      ?n:int ->
      ?ofsy:int ->
      ?incy:int ->
      ?y:vec -> ?ofsx:int -> ?incx:int -> vec -> vec
    val iter :
      (num_type -> unit) ->
      ?n:int -> ?ofsx:int -> ?incx:int -> vec -> unit
    val iteri :
      (int -> num_type -> unit) ->
      ?n:int -> ?ofsx:int -> ?incx:int -> vec -> unit
    val fold :
      ('a -> num_type -> 'a) ->
      'a -> ?n:int -> ?ofsx:int -> ?incx:int -> vec -> 'a
    val rev : vec -> vec
    val max :
      ?n:int -> ?ofsx:int -> ?incx:int -> vec -> num_type
    val min :
      ?n:int -> ?ofsx:int -> ?incx:int -> vec -> num_type
    val sum :
      ?n:int -> ?ofsx:int -> ?incx:int -> vec -> num_type
    val prod :
      ?n:int -> ?ofsx:int -> ?incx:int -> vec -> num_type
    val sqr_nrm2 :
      ?stable:bool -> ?n:int -> ?ofsx:int -> ?incx:int -> vec -> float
    val ssqr :
      ?n:int ->
      ?c:num_type ->
      ?ofsx:int -> ?incx:int -> vec -> num_type
    val sort :
      ?cmp:(num_type -> num_type -> int) ->
      ?decr:bool ->
      ?n:int ->
      ?ofsp:int ->
      ?incp:int ->
      ?p:Lacaml.Common.int_vec ->
      ?ofsx:int -> ?incx:int -> vec -> unit
    val neg :
      ?n:int ->
      ?ofsy:int ->
      ?incy:int ->
      ?y:vec -> ?ofsx:int -> ?incx:int -> vec -> vec
    val add :
      ?n:int ->
      ?ofsz:int ->
      ?incz:int ->
      ?z:vec ->
      ?ofsx:int ->
      ?incx:int ->
      vec -> ?ofsy:int -> ?incy:int -> vec -> vec
    val sub :
      ?n:int ->
      ?ofsz:int ->
      ?incz:int ->
      ?z:vec ->
      ?ofsx:int ->
      ?incx:int ->
      vec -> ?ofsy:int -> ?incy:int -> vec -> vec
    val mul :
      ?n:int ->
      ?ofsz:int ->
      ?incz:int ->
      ?z:vec ->
      ?ofsx:int ->
      ?incx:int ->
      vec -> ?ofsy:int -> ?incy:int -> vec -> vec
    val div :
      ?n:int ->
      ?ofsz:int ->
      ?incz:int ->
      ?z:vec ->
      ?ofsx:int ->
      ?incx:int ->
      vec -> ?ofsy:int -> ?incy:int -> vec -> vec
    val ssqr_diff :
      ?n:int ->
      ?ofsx:int ->
      ?incx:int ->
      vec ->
      ?ofsy:int -> ?incy:int -> vec -> num_type
  end

  module Mat :
  sig
    val create : int -> int -> mat
    val make : int -> int -> num_type -> mat
    val make0 : int -> int -> mat
    val of_array : num_type array array -> mat
    val to_array : mat -> num_type array array
    val of_col_vecs : vec array -> mat
    val to_col_vecs : mat -> vec array
    val as_vec : mat -> vec
    val init_rows :
      int -> int -> (int -> int -> num_type) -> mat
    val init_cols :
      int -> int -> (int -> int -> num_type) -> mat
    val create_mvec : int -> mat
    val make_mvec : int -> num_type -> mat
    val mvec_of_array : num_type array -> mat
    val mvec_to_array : mat -> num_type array
    val from_col_vec : vec -> mat
    val from_row_vec : vec -> mat
    val empty : mat
    val identity : int -> mat
    val of_diag : vec -> mat
    val dim1 : mat -> int
    val dim2 : mat -> int
    val col : mat -> int -> vec
    val copy_row : ?vec:vec -> mat -> int -> vec
    val transpose_copy :
      ?m:int ->
      ?n:int ->
      ?ar:int ->
      ?ac:int -> mat -> ?br:int -> ?bc:int -> mat -> unit
    val transpose :
      ?m:int -> ?n:int -> ?ar:int -> ?ac:int -> mat -> mat
    val detri :
      ?up:bool -> ?n:int -> ?ar:int -> ?ac:int -> mat -> unit
    val packed :
      ?up:bool -> ?n:int -> ?ar:int -> ?ac:int -> mat -> vec
    val unpacked : ?up:bool -> ?n:int -> vec -> mat
    val copy_diag : mat -> vec
    val trace : mat -> num_type
    val scal :
      ?m:int ->
      ?n:int -> num_type -> ?ar:int -> ?ac:int -> mat -> unit
    val scal_cols :
      ?m:int ->
      ?n:int ->
      ?ar:int -> ?ac:int -> mat -> ?ofs:int -> vec -> unit
    val scal_rows :
      ?m:int ->
      ?n:int ->
      ?ofs:int -> vec -> ?ar:int -> ?ac:int -> mat -> unit
    val axpy :
      ?m:int ->
      ?n:int ->
      ?alpha:num_type ->
      ?xr:int ->
      ?xc:int -> x:mat -> ?yr:int -> ?yc:int -> mat -> unit
    val gemm_diag :
      ?n:int ->
      ?k:int ->
      ?beta:num_type ->
      ?ofsy:int ->
      ?y:vec ->
      ?transa:trans3 ->
      ?alpha:num_type ->
      ?ar:int ->
      ?ac:int ->
      mat ->
      ?transb:trans3 ->
      ?br:int -> ?bc:int -> mat -> vec
    val syrk_diag :
      ?n:int ->
      ?k:int ->
      ?beta:num_type ->
      ?ofsy:int ->
      ?y:vec ->
      ?trans:Lacaml.Common.trans2 ->
      ?alpha:num_type ->
      ?ar:int -> ?ac:int -> mat -> vec
    val gemm_trace :
      ?n:int ->
      ?k:int ->
      ?transa:trans3 ->
      ?ar:int ->
      ?ac:int ->
      mat ->
      ?transb:trans3 ->
      ?br:int -> ?bc:int -> mat -> num_type
    val syrk_trace :
      ?n:int ->
      ?k:int -> ?ar:int -> ?ac:int -> mat -> num_type
    val symm2_trace :
      ?n:int ->
      ?upa:bool ->
      ?ar:int ->
      ?ac:int ->
      mat ->
      ?upb:bool -> ?br:int -> ?bc:int -> mat -> num_type
    val map :
      (num_type -> num_type) ->
      ?m:int ->
      ?n:int ->
      ?br:int ->
      ?bc:int ->
      ?b:mat -> ?ar:int -> ?ac:int -> mat -> mat
    val fold_cols :
      ('a -> vec -> 'a) ->
      ?n:int -> ?ac:int -> 'a -> mat -> 'a
  end

  val pp_num : Format.formatter -> num_type -> unit
  val pp_vec : (num_type, 'a) Lacaml.Io.pp_vec
  val pp_mat : (num_type, 'a) Lacaml.Io.pp_mat

  val swap :
    ?n:int ->
    ?ofsx:int ->
    ?incx:int ->
    x:vec -> ?ofsy:int -> ?incy:int -> vec -> unit
  val scal :
    ?n:int ->
    num_type -> ?ofsx:int -> ?incx:int -> vec -> unit
  val copy :
    ?n:int ->
    ?ofsy:int ->
    ?incy:int ->
    ?y:vec -> ?ofsx:int -> ?incx:int -> vec -> vec
  val nrm2 : ?n:int -> ?ofsx:int -> ?incx:int -> vec -> float
  val axpy :
    ?n:int ->
    ?alpha:num_type ->
    ?ofsx:int ->
    ?incx:int ->
    x:vec -> ?ofsy:int -> ?incy:int -> vec -> unit
  val iamax : ?n:int -> ?ofsx:int -> ?incx:int -> vec -> int
  val amax :
    ?n:int -> ?ofsx:int -> ?incx:int -> vec -> num_type
  val gemv :
    ?m:int ->
    ?n:int ->
    ?beta:num_type ->
    ?ofsy:int ->
    ?incy:int ->
    ?y:vec ->
    ?trans:trans3 ->
    ?alpha:num_type ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?ofsx:int -> ?incx:int -> vec -> vec
  val gbmv :
    ?m:int ->
    ?n:int ->
    ?beta:num_type ->
    ?ofsy:int ->
    ?incy:int ->
    ?y:vec ->
    ?trans:trans3 ->
    ?alpha:num_type ->
    ?ar:int ->
    ?ac:int ->
    mat ->
    int -> int -> ?ofsx:int -> ?incx:int -> vec -> vec
  val symv :
    ?n:int ->
    ?beta:num_type ->
    ?ofsy:int ->
    ?incy:int ->
    ?y:vec ->
    ?up:bool ->
    ?alpha:num_type ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?ofsx:int -> ?incx:int -> vec -> vec
  val trmv :
    ?n:int ->
    ?trans:trans3 ->
    ?diag:Lacaml.Common.diag ->
    ?up:bool ->
    ?ar:int ->
    ?ac:int -> mat -> ?ofsx:int -> ?incx:int -> vec -> unit
  val trsv :
    ?n:int ->
    ?trans:trans3 ->
    ?diag:Lacaml.Common.diag ->
    ?up:bool ->
    ?ar:int ->
    ?ac:int -> mat -> ?ofsx:int -> ?incx:int -> vec -> unit
  val tpmv :
    ?n:int ->
    ?trans:trans3 ->
    ?diag:Lacaml.Common.diag ->
    ?up:bool ->
    ?ofsap:int ->
    vec -> ?ofsx:int -> ?incx:int -> vec -> unit
  val tpsv :
    ?n:int ->
    ?trans:trans3 ->
    ?diag:Lacaml.Common.diag ->
    ?up:bool ->
    ?ofsap:int ->
    vec -> ?ofsx:int -> ?incx:int -> vec -> unit
  val gemm :
    ?m:int ->
    ?n:int ->
    ?k:int ->
    ?beta:num_type ->
    ?cr:int ->
    ?cc:int ->
    ?c:mat ->
    ?transa:trans3 ->
    ?alpha:num_type ->
    ?ar:int ->
    ?ac:int ->
    mat ->
    ?transb:trans3 ->
    ?br:int -> ?bc:int -> mat -> mat
  val symm :
    ?m:int ->
    ?n:int ->
    ?side:Lacaml.Common.side ->
    ?up:bool ->
    ?beta:num_type ->
    ?cr:int ->
    ?cc:int ->
    ?c:mat ->
    ?alpha:num_type ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?br:int -> ?bc:int -> mat -> mat
  val trmm :
    ?m:int ->
    ?n:int ->
    ?side:Lacaml.Common.side ->
    ?up:bool ->
    ?transa:trans3 ->
    ?diag:Lacaml.Common.diag ->
    ?alpha:num_type ->
    ?ar:int ->
    ?ac:int -> a:mat -> ?br:int -> ?bc:int -> mat -> unit
  val trsm :
    ?m:int ->
    ?n:int ->
    ?side:Lacaml.Common.side ->
    ?up:bool ->
    ?transa:trans3 ->
    ?diag:Lacaml.Common.diag ->
    ?alpha:num_type ->
    ?ar:int ->
    ?ac:int -> a:mat -> ?br:int -> ?bc:int -> mat -> unit
  val syrk :
    ?n:int ->
    ?k:int ->
    ?up:bool ->
    ?beta:num_type ->
    ?cr:int ->
    ?cc:int ->
    ?c:mat ->
    ?trans:Lacaml.Common.trans2 ->
    ?alpha:num_type ->
    ?ar:int -> ?ac:int -> mat -> mat
  val syr2k :
    ?n:int ->
    ?k:int ->
    ?up:bool ->
    ?beta:num_type ->
    ?cr:int ->
    ?cc:int ->
    ?c:mat ->
    ?trans:Lacaml.Common.trans2 ->
    ?alpha:num_type ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?br:int -> ?bc:int -> mat -> mat
  val lacpy :
    ?uplo:[ `L | `U ] ->
    ?m:int ->
    ?n:int ->
    ?br:int ->
    ?bc:int ->
    ?b:mat -> ?ar:int -> ?ac:int -> mat -> mat
  val lassq :
    ?n:int ->
    ?scale:float ->
    ?sumsq:float -> ?ofsx:int -> ?incx:int -> vec -> float * float
  val larnv :
    ?idist:[ `Normal | `Uniform0 | `Uniform1 ] ->
    ?iseed:Lacaml.Common.int32_vec ->
    ?n:int -> ?ofsx:int -> ?x:vec -> unit -> vec
  val lange_min_lwork : int -> Lacaml.Common.norm4 -> int
  val lange :
    ?m:int ->
    ?n:int ->
    ?norm:Lacaml.Common.norm4 ->
    ?work:rvec -> ?ar:int -> ?ac:int -> mat -> float
  val lauum :
    ?up:bool -> ?n:int -> ?ar:int -> ?ac:int -> mat -> unit
  val getrf :
    ?m:int ->
    ?n:int ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?ar:int -> ?ac:int -> mat -> Lacaml.Common.int32_vec
  val getrs :
    ?n:int ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?trans:trans3 ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val getri_min_lwork : int -> int
  val getri_opt_lwork : ?n:int -> ?ar:int -> ?ac:int -> mat -> int
  val getri :
    ?n:int ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?work:vec -> ?ar:int -> ?ac:int -> mat -> unit
  val sytrf_min_lwork : unit -> int
  val sytrf_opt_lwork :
    ?n:int -> ?up:bool -> ?ar:int -> ?ac:int -> mat -> int
  val sytrf :
    ?n:int ->
    ?up:bool ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?work:vec ->
    ?ar:int -> ?ac:int -> mat -> Lacaml.Common.int32_vec
  val sytrs :
    ?n:int ->
    ?up:bool ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val sytri_min_lwork : int -> int
  val sytri :
    ?n:int ->
    ?up:bool ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?work:vec -> ?ar:int -> ?ac:int -> mat -> unit
  val potrf :
    ?n:int ->
    ?up:bool ->
    ?ar:int -> ?ac:int -> ?jitter:num_type -> mat -> unit
  val potrs :
    ?n:int ->
    ?up:bool ->
    ?ar:int ->
    ?ac:int ->
    mat ->
    ?nrhs:int ->
    ?br:int ->
    ?bc:int ->
    ?factorize:bool -> ?jitter:num_type -> mat -> unit
  val potri :
    ?n:int ->
    ?up:bool ->
    ?ar:int ->
    ?ac:int ->
    ?factorize:bool -> ?jitter:num_type -> mat -> unit
  val trtrs :
    ?n:int ->
    ?up:bool ->
    ?trans:trans3 ->
    ?diag:Lacaml.Common.diag ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val tbtrs :
    ?n:int ->
    ?kd:int ->
    ?up:bool ->
    ?trans:trans3 ->
    ?diag:Lacaml.Common.diag ->
    ?abr:int ->
    ?abc:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val trtri :
    ?n:int ->
    ?up:bool ->
    ?diag:Lacaml.Common.diag -> ?ar:int -> ?ac:int -> mat -> unit
  val geqrf_opt_lwork :
    ?m:int -> ?n:int -> ?ar:int -> ?ac:int -> mat -> int
  val geqrf_min_lwork : n:int -> int
  val geqrf :
    ?m:int ->
    ?n:int ->
    ?work:vec ->
    ?tau:vec -> ?ar:int -> ?ac:int -> mat -> vec
  val gesv :
    ?n:int ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val gbsv :
    ?n:int ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?abr:int ->
    ?abc:int ->
    mat ->
    int -> int -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val gtsv :
    ?n:int ->
    ?ofsdl:int ->
    vec ->
    ?ofsd:int ->
    vec ->
    ?ofsdu:int ->
    vec -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val posv :
    ?n:int ->
    ?up:bool ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val ppsv :
    ?n:int ->
    ?up:bool ->
    ?ofsap:int ->
    vec -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val pbsv :
    ?n:int ->
    ?up:bool ->
    ?kd:int ->
    ?abr:int ->
    ?abc:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val ptsv :
    ?n:int ->
    ?ofsd:int ->
    vec ->
    ?ofse:int ->
    vec -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val sysv_opt_lwork :
    ?n:int ->
    ?up:bool ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> int
  val sysv :
    ?n:int ->
    ?up:bool ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?work:vec ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val spsv :
    ?n:int ->
    ?up:bool ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?ofsap:int ->
    vec -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
  val gels_min_lwork : m:int -> n:int -> nrhs:int -> int
  val gels_opt_lwork :
    ?m:int ->
    ?n:int ->
    ?trans:Lacaml.Common.trans2 ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> int
  val gels :
    ?m:int ->
    ?n:int ->
    ?work:vec ->
    ?trans:Lacaml.Common.trans2 ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> unit
end

module type SDCZ_SD = SDCZ with
    type num_type = float and
    type trans3 = [ `N | `T ]

module type SDCZ_CZ = SDCZ with
    type num_type = Complex.t and
    type trans3 = [ `N | `T | `C ]

(** Signatures of [Lacaml.[SD]]. *)
module type SD =
sig
  include TYPES with
            type num_type = float and
            type trans3 = [ `N | `T ]

  module Vec :
  sig
    val random :
      ?rnd_state:Random.State.t ->
      ?from:float -> ?range:float -> int -> vec
    val sqr :
      ?n:int ->
      ?ofsy:int ->
      ?incy:int ->
      ?y:vec -> ?ofsx:int -> ?incx:int -> vec -> vec
    val sqrt :
      ?n:int ->
      ?ofsy:int ->
      ?incy:int ->
      ?y:vec -> ?ofsx:int -> ?incx:int -> vec -> vec
  end

  module Mat :
  sig
    val hilbert : int -> mat
    val hankel : int -> mat
    val pascal : int -> mat
    val rosser : unit -> mat
    val toeplitz : vec -> mat
    val vandermonde : vec -> mat
    val wilkinson : int -> mat
    val random :
      ?rnd_state:Random.State.t ->
      ?from:float -> ?range:float -> int -> int -> mat
  end

  val dot :
    ?n:int ->
    ?ofsx:int ->
    ?incx:int ->
    x:vec -> ?ofsy:int -> ?incy:int -> vec -> float
  val asum : ?n:int -> ?ofsx:int -> ?incx:int -> vec -> float
  val sbmv :
    ?n:int ->
    ?k:int ->
    ?ofsy:int ->
    ?incy:int ->
    ?y:vec ->
    ?ar:int ->
    ?ac:int ->
    mat ->
    ?up:bool ->
    ?alpha:float ->
    ?beta:float -> ?ofsx:int -> ?incx:int -> vec -> vec
  val ger :
    ?m:int ->
    ?n:int ->
    ?alpha:float ->
    ?ofsx:int ->
    ?incx:int ->
    vec ->
    ?ofsy:int ->
    ?incy:int ->
    vec -> ?ar:int -> ?ac:int -> mat -> mat
  val syr :
    ?n:int ->
    ?alpha:float ->
    ?up:bool ->
    ?ofsx:int ->
    ?incx:int ->
    vec -> ?ar:int -> ?ac:int -> mat -> mat
  val lansy_min_lwork : int -> Lacaml.Common.norm4 -> int
  val lansy :
    ?n:int ->
    ?up:bool ->
    ?norm:Lacaml.Common.norm4 ->
    ?work:vec -> ?ar:int -> ?ac:int -> mat -> float
  val lamch : [ `B | `E | `L | `M | `N | `O | `P | `R | `S | `U ] -> float
  val orgqr_min_lwork : n:int -> int
  val orgqr_opt_lwork :
    ?m:int ->
    ?n:int ->
    ?k:int -> tau:vec -> ?ar:int -> ?ac:int -> mat -> int
  val orgqr :
    ?m:int ->
    ?n:int ->
    ?k:int ->
    ?work:vec ->
    tau:vec -> ?ar:int -> ?ac:int -> mat -> unit
  val ormqr_opt_lwork :
    ?side:Lacaml.Common.side ->
    ?trans:Lacaml.Common.trans2 ->
    ?m:int ->
    ?n:int ->
    ?k:int ->
    tau:vec ->
    ?ar:int ->
    ?ac:int -> mat -> ?cr:int -> ?cc:int -> mat -> int
  val ormqr :
    ?side:Lacaml.Common.side ->
    ?trans:Lacaml.Common.trans2 ->
    ?m:int ->
    ?n:int ->
    ?k:int ->
    ?work:vec ->
    tau:vec ->
    ?ar:int ->
    ?ac:int -> mat -> ?cr:int -> ?cc:int -> mat -> unit
  val gecon_min_lwork : int -> int
  val gecon_min_liwork : int -> int
  val gecon :
    ?n:int ->
    ?norm:Lacaml.Common.norm2 ->
    ?anorm:float ->
    ?work:vec ->
    ?iwork:Lacaml.Common.int32_vec ->
    ?ar:int -> ?ac:int -> mat -> float
  val sycon_min_lwork : int -> int
  val sycon_min_liwork : int -> int
  val sycon :
    ?n:int ->
    ?up:bool ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?anorm:float ->
    ?work:vec ->
    ?iwork:Lacaml.Common.int32_vec ->
    ?ar:int -> ?ac:int -> mat -> float
  val pocon_min_lwork : int -> int
  val pocon_min_liwork : int -> int
  val pocon :
    ?n:int ->
    ?up:bool ->
    ?anorm:float ->
    ?work:vec ->
    ?iwork:Lacaml.Common.int32_vec ->
    ?ar:int -> ?ac:int -> mat -> float
  val gelsy_min_lwork : m:int -> n:int -> nrhs:int -> int
  val gelsy_opt_lwork :
    ?m:int ->
    ?n:int ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> int
  val gelsy :
    ?m:int ->
    ?n:int ->
    ?ar:int ->
    ?ac:int ->
    mat ->
    ?rcond:float ->
    ?jpvt:Lacaml.Common.int32_vec ->
    ?work:vec ->
    ?nrhs:int -> ?br:int -> ?bc:int -> mat -> int
  val gelsd_min_lwork : m:int -> n:int -> nrhs:int -> int
  val gelsd_opt_lwork :
    ?m:int ->
    ?n:int ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> int
  val gelsd_min_iwork : int -> int -> int
  val gelsd :
    ?m:int ->
    ?n:int ->
    ?rcond:float ->
    ?ofss:int ->
    ?s:vec ->
    ?work:vec ->
    ?iwork:vec ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> int
  val gelss_min_lwork : m:int -> n:int -> nrhs:int -> int
  val gelss_opt_lwork :
    ?ar:int ->
    ?ac:int ->
    mat ->
    ?m:int ->
    ?n:int -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> int
  val gelss :
    ?m:int ->
    ?n:int ->
    ?rcond:float ->
    ?ofss:int ->
    ?s:vec ->
    ?work:vec ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?nrhs:int -> ?br:int -> ?bc:int -> mat -> int
  val gesvd_min_lwork : m:int -> n:int -> int
  val gesvd_opt_lwork :
    ?m:int ->
    ?n:int ->
    ?jobu:Lacaml.Common.svd_job ->
    ?jobvt:Lacaml.Common.svd_job ->
    ?s:vec ->
    ?ur:int ->
    ?uc:int ->
    ?u:mat ->
    ?vtr:int ->
    ?vtc:int -> ?vt:mat -> ?ar:int -> ?ac:int -> mat -> int
  val gesvd :
    ?m:int ->
    ?n:int ->
    ?jobu:Lacaml.Common.svd_job ->
    ?jobvt:Lacaml.Common.svd_job ->
    ?s:vec ->
    ?ur:int ->
    ?uc:int ->
    ?u:mat ->
    ?vtr:int ->
    ?vtc:int ->
    ?vt:mat ->
    ?work:vec ->
    ?ar:int ->
    ?ac:int -> mat -> vec * mat * mat
  val gesdd_liwork : m:int -> n:int -> int
  val gesdd_min_lwork :
    ?jobz:Lacaml.Common.svd_job -> m:int -> n:int -> unit -> int
  val gesdd_opt_lwork :
    ?m:int ->
    ?n:int ->
    ?jobz:Lacaml.Common.svd_job ->
    ?s:vec ->
    ?ur:int ->
    ?uc:int ->
    ?u:mat ->
    ?vtr:int ->
    ?vtc:int ->
    ?vt:mat ->
    ?iwork:Lacaml.Common.int32_vec ->
    ?ar:int -> ?ac:int -> mat -> int
  val gesdd :
    ?m:int ->
    ?n:int ->
    ?jobz:Lacaml.Common.svd_job ->
    ?s:vec ->
    ?ur:int ->
    ?uc:int ->
    ?u:mat ->
    ?vtr:int ->
    ?vtc:int ->
    ?vt:mat ->
    ?work:vec ->
    ?iwork:Lacaml.Common.int32_vec ->
    ?ar:int ->
    ?ac:int -> mat -> vec * mat * mat
  val geev_min_lwork : ?vectors:bool -> int -> int
  val geev_opt_lwork :
    ?n:int ->
    ?vlr:int ->
    ?vlc:int ->
    ?vl:mat option ->
    ?vrr:int ->
    ?vrc:int ->
    ?vr:mat option ->
    ?ofswr:int ->
    ?wr:vec ->
    ?ofswi:int ->
    ?wi:vec -> ?ar:int -> ?ac:int -> mat -> int
  val geev :
    ?n:int ->
    ?work:vec ->
    ?vlr:int ->
    ?vlc:int ->
    ?vl:mat option ->
    ?vrr:int ->
    ?vrc:int ->
    ?vr:mat option ->
    ?ofswr:int ->
    ?wr:vec ->
    ?ofswi:int ->
    ?wi:vec ->
    ?ar:int ->
    ?ac:int ->
    mat -> mat * vec * vec * mat
  val syev_min_lwork : int -> int
  val syev_opt_lwork :
    ?n:int ->
    ?vectors:bool -> ?up:bool -> ?ar:int -> ?ac:int -> mat -> int
  val syev :
    ?n:int ->
    ?vectors:bool ->
    ?up:bool ->
    ?work:vec ->
    ?ofsw:int ->
    ?w:vec -> ?ar:int -> ?ac:int -> mat -> vec
  val syevd_min_lwork : vectors:bool -> int -> int
  val syevd_min_liwork : vectors:bool -> int -> int
  val syevd_opt_lwork :
    ?n:int ->
    ?vectors:bool -> ?up:bool -> ?ar:int -> ?ac:int -> mat -> int
  val syevd_opt_liwork :
    ?n:int ->
    ?vectors:bool -> ?up:bool -> ?ar:int -> ?ac:int -> mat -> int
  val syevd_opt_l_li_work :
    ?n:int ->
    ?vectors:bool ->
    ?up:bool -> ?ar:int -> ?ac:int -> mat -> int * int
  val syevd :
    ?n:int ->
    ?vectors:bool ->
    ?up:bool ->
    ?work:vec ->
    ?iwork:Lacaml.Common.int32_vec ->
    ?ofsw:int ->
    ?w:vec -> ?ar:int -> ?ac:int -> mat -> vec
  val sbev_min_lwork : int -> int
  val sbev :
    ?n:int ->
    ?kd:int ->
    ?zr:int ->
    ?zc:int ->
    ?z:mat ->
    ?up:bool ->
    ?work:vec ->
    ?ofsw:int ->
    ?w:vec -> ?abr:int -> ?abc:int -> mat -> vec
  val syevr_min_lwork : int -> int
  val syevr_min_liwork : int -> int
  val syevr_opt_lwork :
    ?n:int ->
    ?vectors:bool ->
    ?range:[ `A | `I of int * int | `V of float * float ] ->
    ?up:bool -> ?abstol:float -> ?ar:int -> ?ac:int -> mat -> int
  val syevr_opt_liwork :
    ?n:int ->
    ?vectors:bool ->
    ?range:[ `A | `I of int * int | `V of float * float ] ->
    ?up:bool -> ?abstol:float -> ?ar:int -> ?ac:int -> mat -> int
  val syevr_opt_l_li_work :
    ?n:int ->
    ?vectors:bool ->
    ?range:[ `A | `I of int * int | `V of float * float ] ->
    ?up:bool ->
    ?abstol:float -> ?ar:int -> ?ac:int -> mat -> int * int
  val syevr :
    ?n:int ->
    ?vectors:bool ->
    ?range:[ `A | `I of int * int | `V of float * float ] ->
    ?up:bool ->
    ?abstol:float ->
    ?work:vec ->
    ?iwork:Lacaml.Common.int32_vec ->
    ?ofsw:int ->
    ?w:vec ->
    ?zr:int ->
    ?zc:int ->
    ?z:mat ->
    ?isuppz:Lacaml.Common.int32_vec ->
    ?ar:int ->
    ?ac:int ->
    mat ->
    int * vec * mat * Lacaml.Common.int32_vec
  val sygv_opt_lwork :
    ?n:int ->
    ?vectors:bool ->
    ?up:bool ->
    ?itype:[ `AB | `A_B | `BA ] ->
    ?ar:int ->
    ?ac:int -> mat -> ?br:int -> ?bc:int -> mat -> int
  val sygv :
    ?n:int ->
    ?vectors:bool ->
    ?up:bool ->
    ?work:vec ->
    ?ofsw:int ->
    ?w:vec ->
    ?itype:[ `AB | `A_B | `BA ] ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?br:int -> ?bc:int -> mat -> vec
  val sbgv :
    ?n:int ->
    ?ka:int ->
    ?kb:int ->
    ?zr:int ->
    ?zc:int ->
    ?z:mat ->
    ?up:bool ->
    ?work:vec ->
    ?ofsw:int ->
    ?w:vec ->
    ?ar:int ->
    ?ac:int ->
    mat -> ?br:int -> ?bc:int -> mat -> vec
end

(** Signatures of [Lacaml.[CZ]]. *)
module type CZ =
sig
  include TYPES with
    type num_type = Complex.t and
    type trans3 = [`N | `T | `C]

  module Vec :
  sig
    val random :
      ?rnd_state:Random.State.t ->
      ?re_from:float ->
      ?re_range:float ->
      ?im_from:float -> ?im_range:float -> int -> vec
  end

  module Mat :
  sig
    val random :
      ?rnd_state:Random.State.t ->
      ?re_from:float ->
      ?re_range:float ->
      ?im_from:float -> ?im_range:float -> int -> int -> mat
  end

  val dotu :
    ?n:int ->
    ?ofsx:int ->
    ?incx:int ->
    x:vec ->
    ?ofsy:int -> ?incy:int -> vec -> num_type
  val dotc :
    ?n:int ->
    ?ofsx:int ->
    ?incx:int ->
    x:vec ->
    ?ofsy:int -> ?incy:int -> vec -> num_type
  val lansy_min_lwork : int -> Lacaml.Common.norm4 -> int
  val lansy :
    ?n:int ->
    ?up:bool ->
    ?norm:Lacaml.Common.norm4 ->
    ?work:rvec -> ?ar:int -> ?ac:int -> mat -> float
  val gecon_min_lwork : int -> int
  val gecon_min_lrwork : int -> int
  val gecon :
    ?n:int ->
    ?norm:Lacaml.Common.norm2 ->
    ?anorm:float ->
    ?work:vec ->
    ?rwork:rvec -> ?ar:int -> ?ac:int -> mat -> float
  val sycon_min_lwork : int -> int
  val sycon :
    ?n:int ->
    ?up:bool ->
    ?ipiv:Lacaml.Common.int32_vec ->
    ?anorm:float ->
    ?work:vec -> ?ar:int -> ?ac:int -> mat -> float
  val pocon_min_lwork : int -> int
  val pocon_min_lrwork : int -> int
  val pocon :
    ?n:int ->
    ?up:bool ->
    ?anorm:float ->
    ?work:vec ->
    ?rwork:rvec -> ?ar:int -> ?ac:int -> mat -> float
  val gesvd_min_lwork : m:int -> n:int -> int
  val gesvd_lrwork : m:int -> n:int -> int
  val gesvd_opt_lwork :
    ?m:int ->
    ?n:int ->
    ?jobu:Lacaml.Common.svd_job ->
    ?jobvt:Lacaml.Common.svd_job ->
    ?s:rvec ->
    ?ur:int ->
    ?uc:int ->
    ?u:mat ->
    ?vtr:int ->
    ?vtc:int -> ?vt:mat -> ?ar:int -> ?ac:int -> mat -> int
  val gesvd :
    ?m:int ->
    ?n:int ->
    ?jobu:Lacaml.Common.svd_job ->
    ?jobvt:Lacaml.Common.svd_job ->
    ?s:rvec ->
    ?ur:int ->
    ?uc:int ->
    ?u:mat ->
    ?vtr:int ->
    ?vtc:int ->
    ?vt:mat ->
    ?work:vec ->
    ?rwork:rvec ->
    ?ar:int ->
    ?ac:int -> mat -> rvec * mat * mat
  val geev_min_lwork : int -> int
  val geev_min_lrwork : int -> int
  val geev_opt_lwork :
    ?n:int ->
    ?vlr:int ->
    ?vlc:int ->
    ?vl:mat option ->
    ?vrr:int ->
    ?vrc:int ->
    ?vr:mat option ->
    ?ofsw:int -> ?w:vec -> ?ar:int -> ?ac:int -> mat -> int
  val geev :
    ?n:int ->
    ?work:vec ->
    ?rwork:vec ->
    ?vlr:int ->
    ?vlc:int ->
    ?vl:mat option ->
    ?vrr:int ->
    ?vrc:int ->
    ?vr:mat option ->
    ?ofsw:int ->
    ?w:vec ->
    ?ar:int ->
    ?ac:int -> mat -> mat * vec * mat
end
