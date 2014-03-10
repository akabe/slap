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

module F (I    : Slap_module_info.SD)
         (SDCZ : Slap_lacaml.SDCZ_SD with type prec = I.prec)
         (SD   : Slap_lacaml.SD      with type prec = I.prec) =
struct
  (* interface: slap_SD_la.ml *)

  include Slap_SDCZ_la_wrap.F(I)(SDCZ)

  (** {2 BLAS interface} *)

  (** {3 Level 1} *)

  let dot ~x:(n, ofsx, incx, x) (n', ofsy, incy, y) =
    assert(n = n');
    SD.dot ~n ~ofsx ~incx ~x ~ofsy ~incy y

  let asum (n, ofsx, incx, x) =
    SD.asum ~n ~ofsx ~incx x

  (** {3 Level 2} *)

  let ger ?(alpha = 1.0) (m, ofsx, incx, x) (n, ofsy, incy, y)
          (m', n', ar, ac, a) =
    assert(m = m' && n = n');
    ignore (SD.ger ~m ~alpha ~ofsx ~incx x ~ofsy ~incy y ~ar ~ac a);
    (m, n, ar, ac, a)

  let syr ?(alpha = 1.0) ?(up = true) (n, ofsx, incx, x)
          (n', n'', ar, ac, a) =
    assert(n = n' && n = n'');
    ignore(SD.syr ~n ~alpha ~up ~ofsx ~incx x ~ar ~ac a);
    (n, n, ar, ac, a)

  (** {2 LAPACK interface} *)

  (** {3 Linear equations (computational routines)} *)

  let orgqr_dyn ~tau:(k, ofs, inc, tau) (m, n, ar, ac, a) =
    assert(ofs = 1 && inc = 1); (* tau must be a cntvector. *)
    if m < n || n < k then invalid_arg "orgqr_dyn";
    SD.orgqr ~m ~n ~k ~tau ~ar ~ac a

  (** {3 Symmetric-matrix eigenvalue and singular value problems
         (simple drivers)} *)

  let syev ?(vectors = false) ?(up = true) ?w (n, n', ar, ac, a) =
    let ofsw, incw, w = default_vec n w in
    assert(n = n');
    assert(ofsw = 1 && incw = 1); (* w must be a cntvector. *)
    ignore (SD.syev ~n ~vectors ~up ~w ~ar ~ac a);
    (n, 1, 1, w)
end
