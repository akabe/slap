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
open Slap_common

type (+'n, 'num, 'prec, +'cnt_or_dsc) t = ('n, 'num, 'prec, 'cnt_or_dsc) vec

let cnt v = __unexpose_vec (__expose_vec v)

let shared_rev v =
  let n, ofsx, incx, x = __expose_vec v in
  let ofsx' = (__expose_size n - 1) * incx + ofsx in
  __unexpose_vec (n, ofsx', -incx, x)

external create_array1 :
  ('a, 'b) kind -> 'n Slap_size.t -> ('a, 'b, fortran_layout) Array1.t
  = "slap_vec_create_array1"

let check_cnt v =
  let n, ofsx, incx, x = __expose_vec v in
  __expose_size n = Array1.dim x && ofsx = 1 && incx = 1

let opt_work = function
  | None -> None
  | Some v ->
    let _, _, _, x = __expose_vec v in
    assert(check_cnt v);
    Some x

let opt_cnt_vec n = function
  | None -> None
  | Some v ->
    let n', _, _, x = __expose_vec v in
    assert(n = n' && check_cnt v);
    Some x

let opt_cnt_vec_alloc kind n = function
  | None -> create_array1 kind n
  | Some v ->
    let n', _, _, x = __expose_vec v in
    assert(n = n' && check_cnt v);
    x

let opt_vec n = function
  | None ->
     None, None, None
  | Some v ->
    let n', ofsx, incx, x = __expose_vec v in
    assert(n = n');
    Some ofsx, Some incx, Some x

let opt_vec_alloc kind n = function
  | None -> 1, 1, create_array1 kind n
  | Some v ->
    let n', ofsx, incx, x = __expose_vec v in
    assert(n = n');
    ofsx, incx, x

(** {2 Creation of vectors} *)

let create kind n =
  __unexpose_vec (n, 1, 1, create_array1 kind n)

let make kind n a =
  let x = create_array1 kind n in
  Array1.fill x a ;
  __unexpose_vec (n, 1, 1, x)

let init kind n f =
  let x = create_array1 kind n in
  Slap_size.iteri (fun i -> Array1.unsafe_set x i (f i)) n;
  __unexpose_vec (n, 1, 1, x)

(** {2 Accessors} *)

let kind v =
  let _, _, _, x = __expose_vec v in
  Array1.kind x

let dim v =
  let n, _, _, _ = __expose_vec v in
  n

let get_dyn v i =
  let n, ofsx, incx, x = __expose_vec v in
  if i < 1 || i > __expose_size n then invalid_arg "Slap.Vec.get_dyn";
  Array1.get x (ofsx + (i - 1) * incx)

let set_dyn v i a =
  let n, ofsx, incx, x = __expose_vec v in
  if i < 1 || i > __expose_size n then invalid_arg "Slap.Vec.set_dyn";
  Array1.set x (ofsx + (i - 1) * incx) a

let unsafe_get v i =
  let _, ofsx, incx, x = __expose_vec v in
  Array1.unsafe_get x (ofsx + (i - 1) * incx)

let unsafe_set v i a =
  let _, ofsx, incx, x = __expose_vec v in
  Array1.unsafe_set x (ofsx + (i - 1) * incx) a

let replace_dyn v i f =
  let n, ofsx, incx, x = __expose_vec v in
  if i < 1 || i > __expose_size n then invalid_arg "Slap.Vec.replace_dyn";
  let j = ofsx + (i - 1) * incx in
  Array1.unsafe_set x j (f (Array1.unsafe_get x j))

(** {2 Iterators} *)

let mapi kind f ?y v =
  let n, ofsx, incx, x = __expose_vec v in
  let ofsy, incy, y = opt_vec_alloc kind n y in
  let rec loop count ix iy =
    if count <= __expose_size n then
      begin
        let xi = Array1.unsafe_get x ix in
        Array1.unsafe_set y iy (f count xi);
        loop (count + 1) (ix + incx) (iy + incy)
      end
  in
  loop 1 ofsx ofsy;
  __unexpose_vec (n, ofsy, incy, y)

let map kind f = mapi kind (fun _ -> f)

let fold_lefti f init v =
  let n, ofsx, incx, x = __expose_vec v in
  let rec loop count i acc =
    if count > __expose_size n then acc else
      begin
        let xi = Array1.unsafe_get x i in
        loop (count + 1) (i + incx) (f count acc xi)
      end
  in
  loop 1 ofsx init

let fold_left f = fold_lefti (fun _ -> f)

let fold_righti f vx init =
  let n = __expose_size (dim vx) + 1 in
  fold_lefti (fun i acc xi -> f (n - i) xi acc) init (shared_rev vx)

let fold_right f = fold_righti (fun _ -> f)

let replace_alli px f =
  ignore (mapi (kind px) f ~y:px px)

let replace_all v f = replace_alli v (fun _ -> f)

let iteri f = fold_lefti (fun i () -> f i) ()

let iter f = iteri (fun _ -> f)

(** {2 Iterators on two vectors} *)

let mapi2 kind f ?z vx vy =
  let n, ofsx, incx, x = __expose_vec vx in
  let n', ofsy, incy, y = __expose_vec vy in
  assert(n = n');
  let ofsz, incz, z = opt_vec_alloc kind n z in
  let rec loop count ix iy iz =
    if count <= __expose_size n then
      begin
        let xi = Array1.unsafe_get x ix in
        let yi = Array1.unsafe_get y iy in
        Array1.unsafe_set z iz (f count xi yi);
        loop (count + 1) (ix + incx) (iy + incy) (iz + incz)
      end
  in
  loop 1 ofsx ofsy ofsz;
  __unexpose_vec (n, ofsz, incz, z)

let map2 kind f = mapi2 kind (fun _ -> f)

let fold_lefti2 f init vx vy =
  let n, ofsx, incx, x = __expose_vec vx in
  let n', ofsy, incy, y = __expose_vec vy in
  assert(n = n');
  let rec loop count ix iy acc =
    if count > __expose_size n then acc else
      begin
        let xi = Array1.unsafe_get x ix in
        let yi = Array1.unsafe_get y iy in
        loop (count + 1) (ix + incx) (iy + incy) (f count acc xi yi)
      end
  in
  loop 1 ofsx ofsy init

let fold_left2 f = fold_lefti2 (fun _ -> f)

let fold_righti2 f vx vy init =
  let n = __expose_size (dim vx) + 1 in
  fold_lefti2 (fun i acc xi yi -> f (n - i) xi yi acc)
              init (shared_rev vx) (shared_rev vy)

let fold_right2 f = fold_righti2 (fun _ -> f)

let iteri2 f = fold_lefti2 (fun i () -> f i) ()

let iter2 f = iteri2 (fun _ -> f)

(** {2 Iterators on three vectors} *)

let mapi3 kind f ?w vx vy vz =
  let n, ofsx, incx, x = __expose_vec vx in
  let n', ofsy, incy, y = __expose_vec vy in
  let n'', ofsz, incz, z = __expose_vec vz in
  assert(n = n' && n = n'');
  let ofsw, incw, w = opt_vec_alloc kind n w in
  let rec loop count ix iy iz iw =
    if count <= __expose_size n then
      begin
        let xi = Array1.unsafe_get x ix in
        let yi = Array1.unsafe_get y iy in
        let zi = Array1.unsafe_get z iz in
        Array1.unsafe_set w iw (f count xi yi zi);
        loop (count + 1) (ix + incx) (iy + incy) (iz + incz) (iw + incw)
      end
  in
  loop 1 ofsx ofsy ofsz ofsw;
  __unexpose_vec (n, ofsw, incw, w)

let map3 kind f = mapi3 kind (fun _ -> f)

let fold_lefti3 f init vx vy vz =
  let n, ofsx, incx, x = __expose_vec vx in
  let n', ofsy, incy, y = __expose_vec vy in
  let n'', ofsz, incz, z = __expose_vec vz in
  assert(n = n' && n = n'');
  let rec loop count ix iy iz acc =
    if count > __expose_size n then acc else
      begin
        let xi = Array1.unsafe_get x ix in
        let yi = Array1.unsafe_get y iy in
        let zi = Array1.unsafe_get z iz in
        loop (count + 1) (ix + incx) (iy + incy) (iz + incz)
             (f count acc xi yi zi)
      end
  in
  loop 1 ofsx ofsy ofsz init

let fold_left3 f = fold_lefti3 (fun _ -> f)

let fold_righti3 f vx vy vz init =
  let n = __expose_size (dim vx) + 1 in
  fold_lefti3 (fun i acc xi yi zi -> f (n - i) xi yi zi acc)
              init (shared_rev vx) (shared_rev vy) (shared_rev vz)

let fold_right3 f = fold_righti3 (fun _ -> f)

let iteri3 f = fold_lefti3 (fun i () -> f i) ()

let iter3 f = iteri3 (fun _ -> f)

(** {2 Scanning} *)

let for_all p vx =
  let n, ofsx, incx, x = __expose_vec vx in
  let rec loop count i =
    if count > __expose_size n then true else
      if p (Array1.unsafe_get x i)
      then loop (count + 1) (i + incx)
      else false
  in
  loop 1 ofsx

let exists p vx = not (for_all (fun xi -> not (p xi)) vx)

let for_all2 p vx vy =
  let n, ofsx, incx, x = __expose_vec vx in
  let n', ofsy, incy, y = __expose_vec vy in
  assert(n = n');
  let rec loop count ix iy =
    if count > __expose_size n then true else
      if p (Array1.unsafe_get x ix) (Array1.unsafe_get y iy)
      then loop (count + 1) (ix + incx) (iy + incy)
      else false
  in
  loop 1 ofsx ofsy

let exists2 p vx vy = not (for_all2 (fun xi yi -> not (p xi yi)) vx vy)

let mem ?(equal=(=)) a = exists (equal a)

(** {2 Basic operations} *)

external copy_stub : n:'n size ->
                     ofsx:int -> incx:int ->
                     ('num, 'prec, fortran_layout) Array1.t ->
                     ofsy:int -> incy:int ->
                     ('num, 'prec, fortran_layout) Array1.t -> unit
  = "slap_vec_copy_stub_bc" "slap_vec_copy_stub"

let copy ?y vx =
  let n, ofsx, incx, x = __expose_vec vx in
  let ofsy, incy, y = opt_vec_alloc (Array1.kind x) n y in
  copy_stub ~n ~ofsx ~incx x ~ofsy ~incy y;
  __unexpose_vec (n, ofsy, incy, y)

external fill_stub : n:'n size ->
                     ofsx:int -> incx:int ->
                     ('num, 'prec, fortran_layout) Array1.t ->
                     'num -> unit
  = "slap_vec_fill_stub"

let fill vx c =
  let n, ofsx, incx, x = __expose_vec vx in
  fill_stub ~n ~ofsx ~incx x c

let append vx vy =
  let m, ofsx, incx, x = __expose_vec vx in
  let n, ofsy, incy, y = __expose_vec vy in
  let k = Slap_size.add m n in
  let z = create_array1 (Array1.kind x) k in
  copy_stub ~n:m ~ofsx ~incx x ~ofsy:1 ~incy:1 z;
  copy_stub ~n ~ofsx:ofsy ~incx:incy y ~ofsy:(__expose_size m + 1) ~incy:1 z;
  __unexpose_vec (k, 1, 1, z)

let rev vx = copy (shared_rev vx)

(** {2 Type conversion} *)

let to_array vx =
  let n, ofsx, incx, x = __expose_vec vx in
  Array.init (__expose_size n) (fun i -> x.{incx * i + ofsx})

let unsafe_of_array kind n arr =
  let x = Array1.of_array kind fortran_layout arr in
  __unexpose_vec (n, 1, 1, x)

let of_array_dyn kind n arr =
  if __expose_size n <> Array.length arr then invalid_arg "Slap.Vec.of_array_dyn";
  unsafe_of_array kind n arr

let to_list x =
  fold_right (fun a acc -> a :: acc) x []

let unsafe_of_list kind n lst =
  let x = create_array1 kind n in
  let f i xi =
    Array1.unsafe_set x i xi;
    i + 1 in
  ignore (List.fold_left f 1 lst);
  __unexpose_vec (n, 1, 1, x)

let of_list_dyn kind n lst =
  if __expose_size n <> List.length lst then invalid_arg "Slap.Vec.of_list_dyn";
  unsafe_of_list kind n lst

let to_bigarray x =
  let ba = create_array1 (kind x) (dim x) in
  iteri (fun i a -> Array1.unsafe_set ba i a) x;
  ba

let unsafe_of_bigarray ?(share=false) n ba =
  let ba' = if share then ba else
      begin
        let ba' = create_array1 (Array1.kind ba) n in
        Array1.blit ba ba';
        ba'
      end in
  __unexpose_vec (n, 1, 1, ba')

let of_bigarray_dyn ?(share=false) n ba =
  if __expose_size n <> Array1.dim ba then invalid_arg "Slap.Vec.of_bigarray_dyn";
  unsafe_of_bigarray ~share n ba

(** {2 Subvectors} *)

let internal_subvec_dyn loc n ofsx incx vx =
  let n', ofsx', incx', x = __expose_vec vx in
  let n' = __expose_size n' in
  (* check the first index *)
  let i1 = ofsx in
  if i1 < 1 || i1 > n' then invalid_arg loc;
  (* check the last index *)
  let iN = ofsx + (__expose_size n - 1) * incx in
  if __expose_size n <> 0 && (iN < 1 || iN > n') then invalid_arg loc;
  __unexpose_vec (n, ofsx' + (ofsx - 1) * incx', incx * incx', x)

let subcntvec_dyn n ?(ofsx = 1) vx =
  let n', _, _, x = __expose_vec vx in
  assert(__expose_size n' >= __expose_size n && check_cnt vx);
  __unexpose_vec (n, 1, 1, Array1.sub x ofsx (__expose_size n))

let subdscvec_dyn n ?(ofsx = 1) ?(incx = 1) vx =
  internal_subvec_dyn "Slap.Vec.subdscvec_dyn" n ofsx incx vx

let subvec_dyn n ?(ofsx = 1) ?(incx = 1) vx =
  internal_subvec_dyn "Slap.Vec.subvec_dyn" n ofsx incx vx
