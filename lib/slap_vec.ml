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

type (+'n, 'num, 'prec, +'cnt_or_dsc) t =
    int   (* the number of elements in a vector (>= 0) *)
    * int (* an offset (>= 1) *)
    * int (* an incrementation *)
    * ('num, 'prec, fortran_layout) Array1.t

let cnt v = v

let create_array1 kind n = Array1.create kind fortran_layout n

let check_cnt n ofsx incx x =
  n = Array1.dim x && ofsx = 1 && incx = 1

let opt_work = function
  | None -> None
  | Some (n, ofsx, incx, x) ->
     assert(check_cnt n ofsx incx x);
     Some x

let opt_cnt_vec n = function
  | None -> None
  | Some (n', ofsx, incx, x) ->
     assert(n = n' && check_cnt n ofsx incx x);
     Some x

let opt_cnt_vec_alloc kind n = function
  | None -> create_array1 kind n
  | Some (n', ofsx, incx, x) ->
     assert(n = n' && check_cnt n ofsx incx x);
     x

let opt_vec n = function
  | None ->
     None, None, None
  | Some (n', ofsx, incx, x) ->
     assert(n = n');
     Some ofsx, Some incx, Some x

let opt_vec_alloc kind n = function
  | None -> 1, 1, create_array1 kind n
  | Some (n', ofsx, incx, x) ->
     assert(n = n');
     ofsx, incx, x

(** {2 Creation of vectors} *)

let create kind n =
  (n, 1, 1, create_array1 kind n)

let make kind n a =
  let x = create_array1 kind n in
  Array1.fill x a ;
  (n, 1, 1, x)

let init kind n f =
  let x = create_array1 kind n in
  Size.iter (fun i -> Array1.unsafe_set x i (f i)) n;
  (n, 1, 1, x)

(** {2 Accessors} *)

let kind (_, _, _, x) = Array1.kind x

let dim (n, _, _, x) = n

let get_dyn (n, ofsx, incx, x) i =
  if i < 1 || i > n then invalid_arg "Slap.Vec.get_dyn";
  Array1.get x (ofsx + (i - 1) * incx)

let set_dyn (n, ofsx, incx, x) i a =
  if i < 1 || i > n then invalid_arg "Slap.Vec.set_dyn";
  Array1.set x (ofsx + (i - 1) * incx) a

let unsafe_get (_, ofsx, incx, x) i =
  Array1.unsafe_get x (ofsx + (i - 1) * incx)

let unsafe_set (_, ofsx, incx, x) i a =
  Array1.unsafe_set x (ofsx + (i - 1) * incx) a

let replace_dyn (n, ofsx, incx, x) i f =
  if i < 1 || i > n then invalid_arg "Slap.Vec.replace_dyn";
  let j = ofsx + (i - 1) * incx in
  Array1.unsafe_set x j (f (Array1.unsafe_get x j))

(** {2 Iterators} *)

let mapi kind f ?y (n, ofsx, incx, x) =
  let ofsy, incy, y = opt_vec_alloc kind n y in
  let rec loop count ix iy =
    if count <= n then
      begin
        let xi = Array1.unsafe_get x ix in
        Array1.unsafe_set y iy (f count xi);
        loop (count + 1) (ix + incx) (iy + incy)
      end
  in
  loop 1 ofsx ofsy;
  (n, ofsy, incy, y)

let map kind f ?y vx =
  mapi kind (fun _ x -> f x) ?y vx

let fold_lefti f init (n, ofsx, incx, x) =
  let rec loop count i acc =
    if count > n then acc else
      begin
        let xi = Array1.unsafe_get x i in
        loop (count + 1) (i + incx) (f count acc xi)
      end
  in
  loop 1 ofsx init

let fold_left f init vx =
  fold_lefti (fun _ acc x -> f acc x) init vx

let fold_righti f (n, ofsx, incx, x) init =
  let rec loop count i acc =
    if count = 0 then acc else
      begin
        let xi = Array1.unsafe_get x i in
        loop (count - 1) (i - incx) (f count xi acc)
      end
  in
  loop n ((n - 1) * incx + ofsx) init

let fold_right f vx init =
  fold_righti (fun _ xi acc -> f xi acc) vx init

let replace_alli px f =
  ignore (mapi (kind px) f ~y:px px)

let replace_all v f =
  replace_alli v (fun _ xi -> f xi)

let iteri f (n, ofsx, incx, x) =
  let rec loop count i =
    if count <= n then begin
      f count (Array1.unsafe_get x i);
      loop (count + 1) (i + incx)
    end
  in
  loop 1 ofsx

let iter f vx =
  iteri (fun _ x -> f x) vx

(** {2 Basic operations} *)

let copy_aux n ofsx incx x ofsy incy y =
  let rec loop count ix iy =
    if count <> 0 then begin
      Array1.unsafe_set y iy (Array1.unsafe_get x ix);
      loop (count - 1) (ix + incx) (iy + incy)
    end
  in
  loop n ofsx ofsy

let copy ?y (n, ofsx, incx, x) =
  let ofsy, incy, y = opt_vec_alloc (Array1.kind x) n y in
  copy_aux n ofsx incx x ofsy incy y;
  (n, ofsy, incy, y)

let fill (n, ofsx, incx, x) c =
  let rec loop count i =
    if count <= n then begin
      Array1.unsafe_set x i c;
      loop (count + 1) (i + incx)
    end
  in
  loop 1 ofsx

let append (m, ofsx, incx, x) (n, ofsy, incy, y) =
  let k = m + n in
  let z = create_array1 (Array1.kind x) k in
  copy_aux m ofsx incx x 1 1 z;
  copy_aux n ofsy incy y (m + 1) 1 z;
  (k, 1, 1, z)

(** {2 Type conversion} *)

let to_array (n, ofsx, incx, x) =
  Array.init n (fun i -> x.{incx * i + ofsx})

let unsafe_of_array kind n arr =
  let x = Array1.of_array kind fortran_layout arr in
  (n, 1, 1, x)

let of_array_dyn kind n arr =
  if n <> Array.length arr then invalid_arg "Slap.Vec.of_array_dyn";
  unsafe_of_array kind n arr

let to_list x =
  fold_right (fun a acc -> a :: acc) x []

let unsafe_of_list kind n lst =
  let x = create_array1 kind n in
  let f i xi =
    Array1.unsafe_set x i xi;
    i + 1 in
  ignore (List.fold_left f 1 lst);
  (n, 1, 1, x)

let of_list_dyn kind n lst =
  if n <> List.length lst then invalid_arg "Slap.Vec.of_list_dyn";
  unsafe_of_list kind n lst

(** {2 Subvectors} *)

let internal_subvec_dyn loc n ofsx incx (n', ofsx', incx', x) =
  (* check the first index *)
  let i1 = ofsx in
  if i1 < 1 || i1 > n' then invalid_arg loc;
  (* check the last index *)
  let iN = ofsx + (n - 1) * incx in
  if n <> 0 && (iN < 1 || iN > n') then invalid_arg loc;
  (n, ofsx' + (ofsx - 1) * incx', incx * incx', x)

let subcntvec_dyn n ?(ofsx = 1) (n', ofsx', incx', x) =
  assert(check_cnt n' ofsx' incx' x);
  (n, 1, 1, Array1.sub x ofsx n)

let subdscvec_dyn n ?(ofsx = 1) ?(incx = 1) vx =
  internal_subvec_dyn "Slap.Vec.subdscvec_dyn" n ofsx incx vx

let subvec_dyn n ?(ofsx = 1) ?(incx = 1) vx =
  internal_subvec_dyn "Slap.Vec.subvec_dyn" n ofsx incx vx
