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

(** {2 Pretty-printing in standard style} *)

type ('n, 'num, 'prec, 'cnt_or_dsc) pp_vec =
    Format.formatter ->
    ('n, 'num, 'prec, 'cnt_or_dsc) Vec.t -> unit
(** A type of standard pretty printers for vectors. *)

val pp_fvec : ('n, float, 'prec, cnt) pp_vec

val pp_cvec : ('n, Complex.t, 'prec, cnt) pp_vec

val pp_ivec : ('n, int32, 'prec, cnt) pp_vec

val pp_rfvec : ('n, float, 'prec, cnt) pp_vec

val pp_rcvec : ('n, Complex.t, 'prec, cnt) pp_vec

val pp_rivec : ('n, int32, 'prec, cnt) pp_vec

type ('m, 'n, 'num, 'prec, 'cnt_or_dsc) pp_mat =
    Format.formatter ->
    ('m, 'n, 'num, 'prec, 'cnt_or_dsc) Mat.t -> unit
(** A type of standard pretty printers for matrices. *)

val pp_fmat : ('m, 'n, float, 'prec, cnt) pp_mat

val pp_cmat : ('m, 'n, Complex.t, 'prec, cnt) pp_mat

val pp_imat : ('m, 'n, int32, 'prec, cnt) pp_mat
