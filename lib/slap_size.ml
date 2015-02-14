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

open Slap_common

type +'n t = 'n size

(** {2 Constants} *)

type z

type 'n s

let zero = __unexpose_size 0

type one = z s

let one = __unexpose_size 1

type two = z s s

let two = __unexpose_size 2

type three = z s s s

let three = __unexpose_size 3

type four = z s s s s

let four = __unexpose_size 4

type five = z s s s s s

let five = __unexpose_size 5

type six = z s s s s s s

let six = __unexpose_size 6

type seven = z s s s s s s s

let seven = __unexpose_size 7

type eight = z s s s s s s s s

let eight = __unexpose_size 8

type nine = z s s s s s s s s s

let nine = __unexpose_size 9

type ten = z s s s s s s s s s s

let ten = __unexpose_size 10

(** {2 Arithmetic operations} *)

let succ n = __unexpose_size (Pervasives.succ (__expose_size n))

type 'n p

let pred_dyn n =
  let n = __expose_size n in
  if n <= 0 then invalid_arg "Slap.Size.pred_dyn";
  __unexpose_size (n - 1)

type ('m, 'n) add

let add m n = __unexpose_size (__expose_size m + __expose_size n)

type ('m, 'n) sub

let sub_dyn m n =
  let m = __expose_size m in
  let n = __expose_size n in
  if m >= n then __unexpose_size (m - n)
  else invalid_arg "Slap.Size.sub_dyn: negative integer"

type ('m, 'n) mul

let mul m n = __unexpose_size (__expose_size m * __expose_size n)

type ('m, 'n) div

let div_dyn m n =
  let m = __expose_size m in
  let n = __expose_size n in
  if n <> 0 then __unexpose_size (m / n)
  else invalid_arg "Slap.Size.div_dyn: zero division"

type ('m, 'n) min

let min m n = __unexpose_size (Pervasives.min (__expose_size m) (__expose_size n))

type ('m, 'n) max

let max m n = __unexpose_size (Pervasives.max (__expose_size m) (__expose_size n))

(** {2 Storage sizes for BLAS and LAPACK} *)

type 'n packed

let packed n =
  let n = __expose_size n in
  __unexpose_size (n * (n + 1) / 2)

let unpacked k =
  let isqrt x = int_of_float (sqrt (float_of_int x) +. 0.5) in
  __unexpose_size ((isqrt (1 + 8 * (__expose_size k)) - 1) / 2)

type ('m, 'n, 'kl, 'ku) geband

let geband_dyn m n kl ku =
  let kl = __expose_size kl in
  let ku = __expose_size ku in
  if kl >= __expose_size m then invalid_arg "Slap.Size.geband_dyn: kl >= m";
  if ku >= __expose_size n then invalid_arg "Slap.Size.geband_dyn: ku >= n";
  __unexpose_size (kl + ku + 1)

type ('n, 'kd) syband

let syband_dyn n kd =
  let kd = __expose_size kd in
  if kd >= __expose_size n then invalid_arg "Slap.Size.syband_dyn: kd >= n";
  __unexpose_size (kd + 1)

type ('m, 'n, 'kl, 'ku) luband = ('m, 'n, 'kl, ('kl, 'ku) add) geband

let luband_dyn m n kl ku =
  let kl = __expose_size kl in
  let ku = __expose_size ku in
  if kl >= __expose_size m then invalid_arg "Slap.Size.luband_dyn: kl >= m";
  if ku >= __expose_size n then invalid_arg "Slap.Size.luband_dyn: ku >= n";
  __unexpose_size (kl + (kl + ku) + 1)

(** {2 Conversion between sizes and integers} *)

module type SIZE =
  sig
    type n
    val value : n t
  end

let to_int = __expose_size

let unsafe_of_int (n : int) =
  let module N =
    struct
      type n
      let value = __unexpose_size n
    end in
  (module N : SIZE)

let of_int_dyn n =
  if n < 0 then invalid_arg "Slap.Size.of_int_dyn";
  unsafe_of_int n

module Of_int_dyn (N : sig val value : int end) : SIZE =
struct
  type n
  let value =
    if N.value < 0 then invalid_arg "Slap.Size.Of_int_dyn";
    __unexpose_size N.value
end

(** {2 Iterators on integers} *)

let fold_lefti f init n =
  let rec loop i e =
    if i > __expose_size n then e else loop (i + 1) (f e i)
  in
  loop 1 init

let fold_righti f n init =
  let rec loop i e =
    if i = 0 then e else loop (i - 1) (f i e)
  in
  loop (__expose_size n) init

let iteri f n = for i = 1 to __expose_size n do f i done

let riteri f n = for i = __expose_size n downto 1 do f i done

(** {2 Iterators on sizes} *)

let fold_left f = fold_lefti (fun acc i -> f acc (unsafe_of_int i))

let fold_right f = fold_righti (fun i -> f (unsafe_of_int i))

let iter f = iteri (fun i -> f (unsafe_of_int i))

let riter f = riteri (fun i -> f (unsafe_of_int i))

(** {2 Checking} *)

let iszero n = (__expose_size n = 0)

let nonzero n = (__expose_size n <> 0)
