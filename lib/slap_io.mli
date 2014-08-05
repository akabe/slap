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

open Format
open Bigarray

type line_type =
  | Head of (formatter -> int -> unit)
  | Foot of (formatter -> int -> unit)
  | Ellipsis
  | Cell of int

val default_ellipsis : string ref
val default_max_rows : int option ref
val default_max_cols : int option ref

val set_max : int -> unit
(** Set both {!Slap.Io.default_max_rows} and {!Slap.Io.default_max_rows}. *)

val pp_table :
  ?pp_open:(formatter -> unit) ->
  ?pp_close:(formatter -> unit) ->
  ?pp_head:(formatter -> int -> unit) ->
  ?pp_foot:(formatter -> int -> unit) ->
  ?pp_end_row:(formatter -> line_type -> unit) ->
  ?pp_end_col:(formatter -> row:line_type -> col:line_type -> unit) ->
  ?pp_left:(formatter -> int -> unit) ->
  ?pp_right:(formatter -> int -> unit) ->
  ?pad:char ->
  ?ellipsis:string ->
  ?max_rows:int option ->
  ?max_cols:int option ->
  formatter ->
  (formatter -> 'el -> unit) ->
  int -> int ->
  (int -> int -> 'el) -> unit

(** {2 Default pretty-printers for elements of vectors or matrices} *)

type 'el pp_el_default = (formatter -> 'el -> unit) ref

val pp_float_el_default : float pp_el_default
(** [fprintf ppf "%G" el] *)

val pp_complex_el_default : Complex.t pp_el_default
(** [fprintf ppf "(%G, %Gi)" x.re x.im] *)

val pp_int32_el_default : int32 pp_el_default
(** [fprintf ppf "%ld" x] *)

(** {2 Pretty-printing in standard style} *)

type ('n, 'num, 'prec, 'cnt_or_dsc) pp_vec =
    formatter -> ('n, 'num, 'prec, 'cnt_or_dsc) Vec.t -> unit
(** A type of standard pretty printers for vectors. *)

val pp_fvec : ('n, float, 'prec, 'cnt_or_dsc) pp_vec

val pp_cvec : ('n, Complex.t, 'prec, 'cnt_or_dsc) pp_vec

val pp_ivec : ('n, int32, 'prec, cnt) pp_vec

val pp_rfvec : ('n, float, 'prec, 'cnt_or_dsc) pp_vec

val pp_rcvec : ('n, Complex.t, 'prec, 'cnt_or_dsc) pp_vec

val pp_rivec : ('n, int32, 'prec, 'cnt_or_dsc) pp_vec

type ('m, 'n, 'num, 'prec, 'cnt_or_dsc) pp_mat =
    formatter -> ('m, 'n, 'num, 'prec, 'cnt_or_dsc) Mat.t -> unit
(** A type of standard pretty printers for matrices. *)

val pp_fmat : ('m, 'n, float, 'prec, 'cnt_or_dsc) pp_mat

val pp_cmat : ('m, 'n, Complex.t, 'prec, 'cnt_or_dsc) pp_mat

val pp_imat : ('m, 'n, int32, 'prec, 'cnt_or_dsc) pp_mat

(** {2 Toplevel pretty-printers} *)

module Toplevel :
sig
  val pp_fvec : ('n, float, 'prec, 'cnt_or_dsc) pp_vec

  val pp_cvec : ('n, Complex.t, 'prec, 'cnt_or_dsc) pp_vec

  val pp_ivec : ('n, int32, 'prec, 'cnt_or_dsc) pp_vec

  val pp_rfvec : ('n, float, 'prec, 'cnt_or_dsc) pp_vec

  val pp_rcvec : ('n, Complex.t, 'prec, 'cnt_or_dsc) pp_vec

  val pp_rivec : ('n, int32, 'prec, 'cnt_or_dsc) pp_vec

  val pp_fmat : ('m, 'n, float, 'prec, 'cnt_or_dsc) pp_mat

  val pp_cmat : ('m, 'n, Complex.t, 'prec, 'cnt_or_dsc) pp_mat

  val pp_imat : ('m, 'n, int32, 'prec, 'cnt_or_dsc) pp_mat
end
