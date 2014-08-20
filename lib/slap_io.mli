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

module Context :
sig
  type t

  val create : int -> t

  val ellipsis_default : string ref
  (** default = ["..."] *)

  val vertical_default : t option ref
  (** - If [Some n], the first [n] rows and the last [n] rows of a table are
        printed. When the number of rows is smaller than [2 * n], all rows are
        shown.
      - If [None], all rows of a table are output.
   *)

  val horizontal_default : t option ref
  (** - If [Some n], the first [n] columns and the last [n] columns of a table
        are printed. When the number of columns is smaller than [2 * n], all
        columns are shown.
      - If [None], all columns of a table are output.
   *)

  val set_dim_defaults : t option -> unit
  (** [set_dim_defaults opt_n] sets both {!Slap.Io.Context.vertical_default} and
      {!Slap.Io.Context.horizontal_default} to [opt_n].
   *)
end

(** {2 General pretty printers} *)

type index =
  | Head
  | Foot
  | Ellipsis
  | Cell of int

val pp_table :
  ?pp_open:(formatter -> unit) ->
  ?pp_close:(formatter -> unit) ->
  ?pp_head:(formatter -> int -> unit) ->
  ?pp_foot:(formatter -> int -> unit) ->
  ?pp_end_row:(formatter -> index -> unit) ->
  ?pp_end_col:(formatter -> row:index -> col:index -> unit) ->
  ?pp_left:(formatter -> int -> unit) ->
  ?pp_right:(formatter -> int -> unit) ->
  ?pad:char ->
  ?ellipsis:string ->
  ?vertical_context:Context.t option ->
  ?horizontal_context:Context.t option ->
  formatter ->
  (formatter -> 'el -> unit) ->
  int -> int ->
  (int -> int -> 'el) -> unit
(** [pp_table
       ?pp_open ?pp_close ?pp_head ?pp_foot ?pp_end_row ?pp_end_col
       ?pp_left ?pp_right ?pad ?ellipsis ?vertical_context ?horizontal_context
       ppf pp_el n_rows n_cols get_el]

    Generic printing of tables.

    - [pp_open ppf] is called when a table is started.
      (default = [pp_open_box ppf 0])
    - [pp_close ppf] is called when a table is complete.
      (default = [pp_close_box ppf ()])
    - [pp_head other_ppf j] is used to print a header for column [j] in a table.
      The first argument [other_ppf] is not [ppf]!
      (default = no header, [1 <= j <= n_cols])
    - [pp_foot other_ppf j] is used to print a footer for column [j] in a
      table. The first argument [other_ppf] is not [ppf]!
      (default = no footer, [1 <= j <= n_cols])
    - [pp_end_row ~row] is called at the end of each row.
      (default = [pp_force_newline ppf ()])
    - [pp_end_col ~col] is called at the end of each column (element).
      (default = [pp_print_string ppf " "])
    - [pp_left other_ppf i] is used to print left labels for row [i] in a table.
      The first argument [other_ppf] is not [ppf]!
      (default = no left labels, [1 <= i <= n_rows])
    - [pp_right other_ppf i] is used to print right labels for row [i] in a
      table. The first argument [other_ppf] is not [ppf]!
      (default = no right labels)
    - [pad] is a padding character for each column. (default = [' '])
    - [ellipsis] is used as a filler when elements need to be skipped.
      (default = [!default_ellipsis])
    - If [vertical_context] is [Some n], the first [n] rows and the last
      [n] rows of a table are printed. When the number of rows is smaller
      than [2 * n], all rows are shown. If [None], all rows of a table are
      output.
      (default = [!Context.vertical_default])
    - If [horizontal_context] is [Some n], the first [n] columns and the last
      [n] columns of a table are printed. When the number of columns is smaller
      than [2 * n], all columns are shown. If [None], all columns of a table are
      output.
      (default = [!Context.horizontal_default])
    - [pp_el] is a pretty printer to used to output elements.
    - [ppf] is the formatter to which all output is finally printed.
    - [n_rows] is the number of all rows of a table.
    - [n_cols] is the number of all columns of a table.
    - [get_el i j] returns the [(i,j)] element of a table.
      ([1 <= i <= n_rows] and [1 <= j <= n_cols])
 *)

val pp_vec_gen :
  ?pp_open:(formatter -> unit) ->
  ?pp_close:(formatter -> unit) ->
  ?pp_head:(formatter -> int -> unit) ->
  ?pp_foot:(formatter -> int -> unit) ->
  ?pp_end_row:(formatter -> index -> unit) ->
  ?pp_end_col:(formatter -> row:index -> col:index -> unit) ->
  ?pp_left:(formatter -> int -> unit) ->
  ?pp_right:(formatter -> int -> unit) ->
  ?pad:char ->
  ?ellipsis:string ->
  ?vertical_context:Context.t option ->
  ?horizontal_context:Context.t option ->
  formatter ->
  (formatter -> 'num -> unit) ->
  ('n, 'num, 'prec, 'cnt_or_dsc) Vec.t -> unit
(** A generator of pretty printers for (column) vectors. *)

val pp_rvec_gen :
  ?pp_open:(formatter -> unit) ->
  ?pp_close:(formatter -> unit) ->
  ?pp_head:(formatter -> int -> unit) ->
  ?pp_foot:(formatter -> int -> unit) ->
  ?pp_end_row:(formatter -> index -> unit) ->
  ?pp_end_col:(formatter -> row:index -> col:index -> unit) ->
  ?pp_left:(formatter -> int -> unit) ->
  ?pp_right:(formatter -> int -> unit) ->
  ?pad:char ->
  ?ellipsis:string ->
  ?vertical_context:int option ->
  ?horizontal_context:int option ->
  formatter ->
  (formatter -> 'num -> unit) ->
  ('n, 'num, 'prec, 'cnt_or_dsc) Vec.t -> unit
(** A generator of pretty printers for row vectors. *)

val pp_mat_gen :
  ?pp_open:(formatter -> unit) ->
  ?pp_close:(formatter -> unit) ->
  ?pp_head:(formatter -> int -> unit) ->
  ?pp_foot:(formatter -> int -> unit) ->
  ?pp_end_row:(formatter -> index -> unit) ->
  ?pp_end_col:(formatter -> row:index -> col:index -> unit) ->
  ?pp_left:(formatter -> int -> unit) ->
  ?pp_right:(formatter -> int -> unit) ->
  ?pad:char ->
  ?ellipsis:string ->
  ?vertical_context:Context.t option ->
  ?horizontal_context:Context.t option ->
  formatter ->
  (formatter -> 'num -> unit) ->
  ('m, 'n, 'num, 'prec, 'cnt_or_dsc) Mat.t -> unit
(** A generator of pretty printers for matrices. *)

(** {2 Default pretty printers for elements of vectors or matrices} *)

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
  val ssc : int -> unit
  (** {i SLAP Set Context}: [ssc n] sets sets both
      {!Slap.Io.Context.vertical_default} and
      {!Slap.Io.Context.horizontal_default} to [Some n].
      This is the shortcut version of {Slap.Io.Context.set_dim_defaults}.
   *)

  val lsc : int -> unit
  (** An alias of [ssc] for compatibility with Lacaml.
      ({i Lacaml Set Context})
   *)

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
