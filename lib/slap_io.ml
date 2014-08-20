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

module Context =
  struct
    type t = int

    let create n =
      if n < 1 then failwith "Slap.Io.Context.create: n < 1"
      else n

    let ellipsis_default = ref "..."

    let horizontal_default, vertical_default =
      let opt_n = if !Sys.interactive then Some 3 else None in
      ref opt_n, ref opt_n

    let set_dim_defaults opt_n =
      horizontal_default := opt_n;
      vertical_default   := opt_n
  end

(** {2 Pretty printers} *)

type index =
  | Head
  | Foot
  | Ellipsis
  | Cell of int

let default_pp_open ppf = pp_open_box ppf 0
let default_pp_close ppf = pp_close_box ppf ()
let default_pp_end_row ppf _ = pp_force_newline ppf ()
let default_pp_end_col ppf ~row:_ ~col:_ = pp_print_string ppf " "
let default_pad = ' '

let make_index_array ?pp_head ?pp_foot context n =
  let idx = match context with
    | Some k when n > 2 * k -> (* omit columns or rows *)
       let offset = n - 2 * k in
       Array.init (2 * k + 1)
                  (fun i -> if i < k then Cell (i + 1)
                            else if i = k then Ellipsis
                            else Cell (i + offset))
    | _ -> Array.init n (fun i -> Cell (i + 1)) (* show all columns or rows *)
  in
  let add_if_some pp_print f idx =
    match pp_print with Some pp_print -> f pp_print idx | None -> idx in
  idx
  |> add_if_some pp_head (fun pp_head idx -> Array.append [|Head|] idx)
  |> add_if_some pp_foot (fun pp_foot idx -> Array.append idx [|Foot|])

let pp_dummy ppf = assert(false)
(* This is a dummy pretty printer. This is never called. *)

let stringize_table ?(pp_head = pp_dummy)
                    ?(pp_foot = pp_dummy)
                    ?(pp_left = pp_dummy)
                    ?(pp_right = pp_dummy)
                    ~ellipsis ~idx_m ~idx_n m n pp_el =
  let buf = Buffer.create 32 in
  let ppf_buf = formatter_of_buffer buf in
  let stringize_pp pp =
    Buffer.clear buf;
    pp ppf_buf;
    pp_print_flush ppf_buf ();
    Buffer.contents buf
  in
  let stringize_cell i = function
    | Head     -> stringize_pp (fun ppf -> pp_left ppf i)
    | Foot     -> stringize_pp (fun ppf -> pp_right ppf i)
    | Cell j   -> stringize_pp (fun ppf -> pp_el ppf i j)
    | Ellipsis -> ellipsis
  in
  let stringize_head_foot pp = function
    | Head     -> ""
    | Foot     -> ""
    | Cell j   -> stringize_pp (fun ppf -> pp ppf j)
    | Ellipsis -> ellipsis
  in
  let stringize_row = function
    | Head     -> Array.map (stringize_head_foot pp_head) idx_n
    | Foot     -> Array.map (stringize_head_foot pp_foot) idx_n
    | Cell i   -> Array.map (stringize_cell i) idx_n
    | Ellipsis -> Array.make (Array.length idx_n) ellipsis
  in
  Array.map stringize_row idx_m

let calc_column_width str_tbl =
  let w = Array.make (Array.length str_tbl.(0)) 0 in
  let update = Array.iteri (fun j el -> w.(j)<- max w.(j) (String.length el)) in
  Array.iter update str_tbl;
  w

let pp_print_table ppf ~pp_end_row ~pp_end_col ~pad
                   ~idx_m ~idx_n width str_tbl =
  let last_row = idx_m.(Array.length idx_m - 1) in
  let pp_print_row i row =
    Array.iteri (fun j col ->
                 let s = str_tbl.(i).(j) in
                 let d = width.(j) - String.length s in
                 pp_print_string ppf (String.make d pad);
                 pp_print_string ppf s;
                 pp_end_col ppf ~row ~col)
                idx_n;
    if last_row <> row then pp_end_row ppf row
  in
  Array.iteri pp_print_row idx_m

let pp_table ?(pp_open = default_pp_open)
             ?(pp_close = default_pp_close)
             ?pp_head
             ?pp_foot
             ?(pp_end_row = default_pp_end_row)
             ?(pp_end_col = default_pp_end_col)
             ?pp_left
             ?pp_right
             ?(pad = default_pad)
             ?(ellipsis = !Context.ellipsis_default)
             ?(vertical_context = !Context.vertical_default)
             ?(horizontal_context = !Context.horizontal_default)
             ppf pp_el m n get_el =
  if m > 0 && n > 0 then
    begin
      let pp_el ppf i j = pp_el ppf (get_el i j) in
      let idx_m = make_index_array ?pp_head:pp_head ?pp_foot:pp_foot
                                   vertical_context m in
      let idx_n = make_index_array ?pp_head:pp_left ?pp_foot:pp_right
                                   horizontal_context n in
      let str_tbl = stringize_table ?pp_head ?pp_foot ?pp_left ?pp_right
                                    ~ellipsis ~idx_m ~idx_n m n pp_el in
      let width = calc_column_width str_tbl in
      pp_open ppf;
      pp_print_table ppf ~pp_end_row ~pp_end_col ~pad ~idx_m ~idx_n
                     width str_tbl;
      pp_close ppf
    end

let pp_vec_gen ?pp_open ?pp_close ?pp_head ?pp_foot ?pp_end_row ?pp_end_col
               ?pp_left ?pp_right ?pad ?ellipsis
               ?vertical_context ?horizontal_context
               ppf pp_el x =
  pp_table ?pp_open ?pp_close ?pp_head ?pp_foot ?pp_end_row ?pp_end_col
           ?pp_left ?pp_right ?pad ?ellipsis
           ?vertical_context ?horizontal_context
           ppf pp_el
           (Vec.dim x) 1 (fun i _ -> Vec.get_dyn x i)

let pp_rvec_gen ?pp_open ?pp_close ?pp_head ?pp_foot ?pp_end_row ?pp_end_col
                ?pp_left ?pp_right ?pad ?ellipsis
                ?vertical_context ?horizontal_context
                ppf pp_el x =
  pp_table ?pp_open ?pp_close ?pp_head ?pp_foot ?pp_end_row ?pp_end_col
           ?pp_left ?pp_right ?pad ?ellipsis
           ?vertical_context ?horizontal_context
           ppf pp_el
           1 (Vec.dim x) (fun _ j -> Vec.get_dyn x j)

let pp_mat_gen ?pp_open ?pp_close ?pp_head ?pp_foot ?pp_end_row ?pp_end_col
               ?pp_left ?pp_right ?pad ?ellipsis
               ?vertical_context ?horizontal_context
               ppf pp_el a =
  let m, n = Mat.dim a in
  pp_table ?pp_open ?pp_close ?pp_head ?pp_foot ?pp_end_row ?pp_end_col
           ?pp_left ?pp_right ?pad ?ellipsis
           ?vertical_context ?horizontal_context
           ppf pp_el
           m n (Mat.get_dyn a)

(** {2 Default pretty-printers for elements of vectors or matrices} *)

type 'el pp_el_default = (formatter -> 'el -> unit) ref

let pp_float_el_default : float pp_el_default = ref (fun ppf -> fprintf ppf "%G")

let pp_complex_el_default = ref (fun ppf {Complex.re=re; Complex.im=im} ->
                                 fprintf ppf "(%G, %Gi)" re im)

let pp_int32_el_default = ref (fun ppf -> fprintf ppf "%ld")

(** {2 Pretty-printing in standard style} *)

type ('n, 'num, 'prec, 'cnt_or_dsc) pp_vec =
    formatter -> ('n, 'num, 'prec, 'cnt_or_dsc) Vec.t -> unit

let pp_fvec ppf x = pp_vec_gen ppf (!pp_float_el_default) x
let pp_cvec ppf x = pp_vec_gen ppf (!pp_complex_el_default) x
let pp_ivec ppf x = pp_vec_gen ppf (!pp_int32_el_default) x

let pp_rfvec ppf x = pp_rvec_gen ppf (!pp_float_el_default) x
let pp_rcvec ppf x = pp_rvec_gen ppf (!pp_complex_el_default) x
let pp_rivec ppf x = pp_rvec_gen ppf (!pp_int32_el_default) x

type ('m, 'n, 'num, 'prec, 'cnt_or_dsc) pp_mat =
    formatter -> ('m, 'n, 'num, 'prec, 'cnt_or_dsc) Mat.t -> unit

let pp_fmat ppf a = pp_mat_gen ppf (!pp_float_el_default) a
let pp_cmat ppf a = pp_mat_gen ppf (!pp_complex_el_default) a
let pp_imat ppf a = pp_mat_gen ppf (!pp_int32_el_default) a

(** {2 Toplevel pretty-printers} *)

module Toplevel =
  struct
    let ssc n = Context.set_dim_defaults (Some (Context.create n))
    let lsc = ssc

    let pp_labeled_row ppf i = fprintf ppf "R%d" i
    let pp_labeled_col ppf j = fprintf ppf "C%d" j

    (* Vectors *)

    let gen_pp_vec ppf pp_el x =
      pp_vec_gen ~pp_left:pp_labeled_row ppf pp_el x

    let pp_fvec ppf x = gen_pp_vec ppf (!pp_float_el_default) x
    let pp_cvec ppf x = gen_pp_vec ppf (!pp_complex_el_default) x
    let pp_ivec ppf x = gen_pp_vec ppf (!pp_int32_el_default) x

    let gen_pp_rvec ppf pp_el x =
      pp_rvec_gen ~pp_head:pp_labeled_row ppf pp_el x

    let pp_rfvec ppf x = gen_pp_rvec ppf (!pp_float_el_default) x
    let pp_rcvec ppf x = gen_pp_rvec ppf (!pp_complex_el_default) x
    let pp_rivec ppf x = gen_pp_rvec ppf (!pp_int32_el_default) x

    (* Matrices *)

    let gen_pp_mat ppf pp_el a =
      pp_mat_gen ~pp_head:pp_labeled_col ~pp_left:pp_labeled_row ppf pp_el a

    let pp_fmat ppf a = gen_pp_mat ppf (!pp_float_el_default) a
    let pp_cmat ppf a = gen_pp_mat ppf (!pp_complex_el_default) a
    let pp_imat ppf a = gen_pp_mat ppf (!pp_int32_el_default) a
  end
