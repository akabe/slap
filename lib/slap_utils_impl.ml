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

(* interface: slap_utils.ml *)

module Common = Slap_common_impl

(** {2 Numerical utilities} *)

let cmp_float ?(epsilon = 1e-6) x y = abs_float (x -. y) < epsilon

let cmp_complex ?(epsilon = 1e-6) x y =
  let open Complex in
  norm2 (sub x y) < epsilon *. epsilon

(** {2 Matrix dimensions} *)

let dim_array_array aa =
  let for_all f a =
    Array.fold_left (fun b e -> if b then f e else false) true a in
  let m = Array.length aa in
  let n = if m = 0 then 0 else Array.length aa.(0) in
  if for_all (fun a -> Array.length a = n) aa then Some (m, n) else None

let dim_list_list = function
  | [] -> Some (0,0)
  | l::rest as ll ->
    let m = List.length ll in
    let n = List.length l in
    if List.for_all (fun l -> List.length l = n) rest then Some (m,n) else None
