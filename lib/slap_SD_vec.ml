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

(** {2 Creation of vectors} *)

let random ?rnd_state ?from ?range n =
  let x = I.Vec.random ?rnd_state ?from ?range (S.__expose n) in
  V.__unexpose n 1 x

(** {2 Arithmetic operations} *)

let sqr ?y x = wrap2opt I.Vec.sqr ?y x

let sqrt ?y x = wrap2opt I.Vec.sqrt ?y x

let exp ?y x = wrap2opt I.Vec.exp ?y x

let log ?y x = wrap2opt I.Vec.log ?y x

let sin ?y x = wrap2opt I.Vec.sin ?y x

let cos ?y x = wrap2opt I.Vec.cos ?y x
