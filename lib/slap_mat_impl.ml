(* Sized Linear Algebra Package (SLAP)

  Copyright (C) 2013- Akinori ABE <abe@kb.ecei.tohoku.ac.jp>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* interface: slap_mat.ml *)

module Common = Slap_common_impl
module Size = Slap_size_impl
module Utils = Slap_utils_impl

open Bigarray
open Common

let check_cnt m n ar ac a =
  m = Array2.dim1 a && n = Array2.dim2 a && ar = 1 && ac = 1

(** {2 Accessors} *)

let kind (_, _, _, _, a) = Array2.kind a

let dim (m, n, _, _, _) = (m, n)

let dim1 (m, _, _, _, _) = m

let dim2 (_, n, _, _, _) = n

let check_bounds m n i j loc =
  if 1 > i || i > m || 1 > j || j > n then invalid_arg loc

let get_dyn (m, n, ar, ac, a) i j =
  check_bounds m n i j "Slap.Mat.get_dyn";
  Array2.get a (i + ar - 1) (j + ac - 1)

let set_dyn (m, n, ar, ac, a) i j x =
  check_bounds m n i j "Slap.Mat.set_dyn";
  Array2.set a (i + ar - 1) (j + ac - 1) x

let unsafe_get (_, _, ar, ac, a) i j =
  Array2.unsafe_get a (i + ar - 1) (j + ac - 1)

let unsafe_set (_, _, ar, ac, a) i j x =
  Bigarray.Array2.unsafe_set a (i + ar - 1) (j + ac - 1) x

let replace_dyn (m, n, ar, ac, a) i j ~f =
  if i < 1 || i > m || j < 1 || j > n then invalid_arg "Slap.Mat.replace_dyn";
  let i = i + ar - 1 in
  let j = j + ac - 1 in
  Array2.unsafe_set a i j (f (Array2.unsafe_get a i j))

let reshape a =
  let m = Array2.dim1 a in
  let n = Array2.dim2 a in
  reshape_1 (genarray_of_array2 a) (m * n)

let col_dyn (m, n, ar, ac, a) i =
  if i < 1 || i > n then invalid_arg "Slap.Mat.col_dyn";
  let lda = Array2.dim1 a in
  (m, lda * (i + ac - 2) + ar, 1, reshape a)

let row_dyn (m, n, ar, ac, a) i =
  if i < 1 || i > m then invalid_arg "Slap.Mat.row_dyn";
  let lda = Array2.dim1 a in
  (n, lda * (ac - 1) + ar + i - 1, lda, reshape a)

let diag (m, n, ar, ac, a) =
  let lda = Array2.dim1 a in
  (min m n, lda * (ac - 1) + ar, lda + 1, reshape a)

let as_vec (m, n, ar, ac, a) =
  assert(check_cnt m n ar ac a);
  (m * n, 1, 1, reshape a)

(** {2 Creation of matrices} *)

let create_bigarray kind m n =
  let layout = fortran_layout in
  Array2.create kind layout m n

let create kind m n =
  (m, n, 1, 1, create_bigarray kind m n)

let make kind m n x =
  let a = create_bigarray kind m n in
  Array2.fill a x;
  (m, n, 1, 1, a)

let init_cols kind m n ~f =
  let a = create_bigarray kind m n in
  for j = 1 to n do
    for i = 1 to m do
      Array2.unsafe_set a i j (f i j)
    done
  done;
  (m, n, 1, 1, a)

let init_rows kind m n ~f =
  let a = create_bigarray kind m n in
  for i = 1 to m do
    for j = 1 to n do
      Array2.unsafe_set a i j (f i j)
    done
  done;
  (m, n, 1, 1, a)

let default_mat kind m n = function
    | None ->
       (1, 1, create_bigarray kind m n)
    | Some (m', n', ar, ac, a) ->
       assert(m = m' && n = n');
       (ar, ac, a)

(** {2 Basic operations} *)

let fill (m, n, ar, ac, a) x =
  for j = 0 to n - 1 do
    for i = 0 to m - 1 do
      Array2.unsafe_set a (i + ar) (j + ac) x
    done
  done

let copy ?b (m, n, ar, ac, a) =
  let br, bc, b = default_mat (Array2.kind a) m n b in
  for j = 0 to n - 1 do
    for i = 0 to m - 1 do
      let e = Array2.unsafe_get a (i + ar) (j + ac) in
      Array2.unsafe_set b (i + br) (j + bc) e
    done
  done;
  (m, n, br, bc, b)

(** {2 Iterators} *)

let replace_all (m, n, ar, ac, a) ~f =
  for j = ac to n + ac - 1 do
    for i = ar to m + ar - 1 do
      Array2.unsafe_set a i j (f (Array2.unsafe_get a i j))
    done
  done

let replace_alli (m, n, ar, ac, a) ~f =
  let ar = ar - 1 in
  let ac = ac - 1 in
  for j = 1 to n do
    let y = j + ac in
    for i = 1 to m do
      let x = i + ar in
      let e = Array2.unsafe_get a x y in
      Array2.unsafe_set a x y (f x y e)
    done
  done

(** {2 Type conversion} *)

let to_array (m, n, ar, ac, a) =
  Array.init m (fun i -> Array.init n (fun j -> a.{i+ar,j+ac}))

let unsafe_of_array kind m n aa =
  assert(Utils.dim_array_array aa <> None);
  (m, n, 1, 1, Bigarray.Array2.of_array kind Bigarray.fortran_layout aa)

let of_array_dyn kind m n aa =
  match Utils.dim_array_array aa with
  | Some (m', n') when m = m' && n = n' -> unsafe_of_array kind m n aa
  | _ -> invalid_arg "Slap.Mat.of_array_dyn"

let to_list (m, n, ar, ac, a) =
  let ar = ar - 1 in
  let ac = ac - 1 in
  let col_to_list i =
    Size.fold_right ~f:(fun j l -> a.{i+ar, j+ac} :: l) ~init:[] n
  in
  Size.fold_right ~f:(fun i ll -> (col_to_list i) :: ll) ~init:[] m

let unsafe_of_list kind m n ll =
  assert(Utils.dim_list_list ll <> None);
  let mat = create_bigarray kind m n in
  let list_iteri f l = ignore (List.fold_left (fun i x -> f i x; i + 1) 1 l) in
  list_iteri (fun i l -> list_iteri (fun j x -> mat.{i,j} <- x) l) ll;
  (m, n, 1, 1, mat)

let of_list_dyn kind m n ll =
  match Utils.dim_list_list ll with
  | Some (m', n') when m = m' && n = n' -> unsafe_of_list kind m n ll
  | _ -> invalid_arg "Slap.Mat.of_list_dyn"

(** {2 Submatrices} *)

let submat_dyn m n ?(ar=1) ?(ac=1) (m', n', ar', ac', a) =
  if ar < 1 || ar > m' || m' < m + ar - 1 ||
     ac < 1 || ac > n' || n' < n + ac - 1
    then invalid_arg "Slap.Mat.submat_dyn" ;
  (m, n, ar + ar' - 1, ac + ac' - 1, a)
