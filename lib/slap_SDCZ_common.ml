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

module I = Lacaml.XSDCZ

module S = Slap_size
module V = Slap_vec
module M = Slap_mat

type (+'n, +'cnt_or_dsc) vec = ('n, num_type, prec, 'cnt_or_dsc) V.t

type (+'m, +'n, +'cnt_or_dsc) mat = ('m, 'n, num_type, prec, 'cnt_or_dsc) M.t

type rprec = floatXBITS_elt

type (+'n, +'cnt_or_dsc) rvec = ('n, float, rprec, 'cnt_or_dsc) V.t

let rprec = floatXBITS

let invalid_argf fmt =
  Printf.kprintf (fun s () -> invalid_arg ("Slap.XSDCZ." ^ s)) fmt

let (|>) x f = f x (* for OCaml 4.00 or below *)
