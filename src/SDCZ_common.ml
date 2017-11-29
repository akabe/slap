(* Sized Linear Algebra Package (SLAP)

   Copyright (C) 2013- Akinori ABE <aabe.65535@gmail.com>

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

module S = Size
module V = Vec
module M = Mat

type (+'n, +'cnt_or_dsc) vec = ('n, num_type, prec, 'cnt_or_dsc) V.t

type (+'m, +'n, +'cnt_or_dsc) mat = ('m, 'n, num_type, prec, 'cnt_or_dsc) M.t

type rprec = CONCAT(CONCAT(float, XBITS), _elt)

type (+'n, +'cnt_or_dsc) rvec = ('n, float, rprec, 'cnt_or_dsc) V.t

let rprec = CONCAT(float, XBITS)

let invalid_argf fmt =
  Printf.kprintf (fun s () -> invalid_arg ("Slap.XSDCZ." ^ s)) fmt

let (|>) x f = f x (* for OCaml 4.00 or below *)
