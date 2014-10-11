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

open Bigarray

type (+'m, +'n, 'num, 'prec, +'cnt_or_dsc) t =
    int   (* the number of rows in a matrix    (>= 0) *)
    * int (* the number of columns in a matrix (>= 0) *)
    * int (* an offset of rows    (>= 1) *)
    * int (* an offset of columns (>= 1) *)
    * ('num, 'prec, fortran_layout) Array2.t

let cnt a = a

let check_cnt m n ar ac a =
  m = Array2.dim1 a && n = Array2.dim2 a && ar = 1 && ac = 1

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
    if List.for_all (fun l -> List.length l = n) rest then Some (m, n) else None

let create_bigarray kind m n =
  Array2.create kind (fortran_layout) m n

let opt_mat m n = function
  | None -> (None, None, None)
  | Some (m', n', ar, ac, a) ->
     assert(m = m' && n = n');
     (Some ar, Some ac, Some a)

let opt_mat_alloc kind m n = function
  | None ->
     (1, 1, create_bigarray kind m n)
  | Some (m', n', ar, ac, a) ->
     assert(m = m' && n = n');
     (ar, ac, a)

(** {2 Creation of matrices} *)

let create kind m n =
  (m, n, 1, 1, create_bigarray kind m n)

let make kind m n x =
  let a = create_bigarray kind m n in
  Array2.fill a x;
  (m, n, 1, 1, a)

let init_cols kind m n f =
  let a = create_bigarray kind m n in
  for j = 1 to n do
    for i = 1 to m do
      Array2.unsafe_set a i j (f i j)
    done
  done;
  (m, n, 1, 1, a)

let init_rows kind m n f =
  let a = create_bigarray kind m n in
  for i = 1 to m do
    for j = 1 to n do
      Array2.unsafe_set a i j (f i j)
    done
  done;
  (m, n, 1, 1, a)

let init = init_cols

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

let replace_dyn (m, n, ar, ac, a) i j f =
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

let diag (n, n', ar, ac, a) =
  assert(n = n');
  let lda = Array2.dim1 a in
  (n, lda * (ac - 1) + ar, lda + 1, reshape a)

let as_vec (m, n, ar, ac, a) =
  assert(check_cnt m n ar ac a);
  (m * n, 1, 1, reshape a)

(** {2 Basic operations} *)

external fill_stub : m:int -> n:int ->
                     ar:int -> ac:int ->
                     ('num, 'prec, fortran_layout) Array2.t ->
                     'num -> unit
  = "slap_mat_fill_stub_bc" "slap_mat_fill_stub"

let fill (m, n, ar, ac, a) x =
  fill_stub ~m ~n ~ar ~ac a x

external copy_stub : m:int -> n:int ->
                     ar:int -> ac:int ->
                     ('num, 'prec, fortran_layout) Array2.t ->
                     br:int -> bc:int ->
                     ('num, 'prec, fortran_layout) Array2.t -> unit
  = "slap_mat_copy_stub_bc" "slap_mat_copy_stub"

let copy ?b (m, n, ar, ac, a) =
  let br, bc, b = opt_mat_alloc (Array2.kind a) m n b in
  copy_stub ~m ~n ~ar ~ac a ~br ~bc b;
  (m, n, br, bc, b)

(** {2 Matrix transformations} *)

external packed_stub : n:int -> up:bool ->
                       ('num, 'prec, fortran_layout) Array1.t ->
                       ar:int -> ac:int ->
                       ('num, 'prec, fortran_layout) Array2.t -> unit
  = "slap_mat_packed_stub_bc" "slap_mat_packed_stub"

let packed ?(up = true) ?x (n, n', ar, ac, a) =
  assert(n = n');
  let k = Size.packed n in
  let x = Vec.opt_cnt_vec_alloc (Array2.kind a) k x in
  packed_stub ~up ~n x ~ar ~ac a;
  (k, 1, 1, x)

external unpacked_stub : n:int -> up:bool ->
                         fill_num:'num option ->
                         ('num, 'prec, fortran_layout) Array1.t ->
                         ar:int -> ac:int ->
                         ('num, 'prec, fortran_layout) Array2.t -> unit
  = "slap_mat_unpacked_stub_bc" "slap_mat_unpacked_stub"

let unpacked ?(up = true) ?(fill_num = None) ?a (k, ofsx, incx, x) =
  assert(Vec.check_cnt k ofsx incx x);
  let n = Size.unpacked k in
  let ar, ac, a = opt_mat_alloc (Array1.kind x) n n a in
  unpacked_stub ~n ~up ~fill_num x ~ar ~ac a;
  (n, n, ar, ac, a)

external geband_stub : m:int -> n:int ->
                       kl:int -> ku:int ->
                       ar:int -> ac:int ->
                       ('num, 'prec, fortran_layout) Array2.t ->
                       br:int -> bc:int ->
                       ('num, 'prec, fortran_layout) Array2.t -> unit
  = "slap_mat_geband_stub_bc" "slap_mat_geband_stub"

let geband_dyn kl ku ?b (m, n, ar, ac, a) =
  if kl >= m then invalid_arg "Slap.Mat.geband_dyn: kl >= m";
  if ku >= n then invalid_arg "Slap.Mat.geband_dyn: ku >= n";
  let gbsize = kl + ku + 1 in
  let br, bc, b = opt_mat_alloc (Array2.kind a) gbsize n b in
  geband_stub ~m ~n ~kl ~ku ~ar ~ac a ~br ~bc b;
  (gbsize, n, br, bc, b)

external ungeband_stub : m:int -> n:int ->
                         kl:int -> ku:int ->
                         fill_num:'num option ->
                         ar:int -> ac:int ->
                         ('num, 'prec, fortran_layout) Array2.t ->
                         br:int -> bc:int ->
                         ('num, 'prec, fortran_layout) Array2.t -> unit
  = "slap_mat_ungeband_stub_bc" "slap_mat_ungeband_stub"

let ungeband m kl ku ?(fill_num = None) ?a (gbsize, n, br, bc, b) =
  assert(gbsize = kl + ku + 1);
  let ar, ac, a = opt_mat_alloc (Array2.kind b) m n a in
  ungeband_stub ~m ~n ~kl ~ku ~fill_num ~ar ~ac a ~br ~bc b;
  (m, n, ar, ac, a)

let syband_dyn kd ?(up = true) ?b (n, n', ar, ac, a) =
  assert(n = n');
  if kd >= n then invalid_arg "Slap.Mat.syband_dyn: kd >= n";
  let sbsize = kd + 1 in
  let br, bc, b = opt_mat_alloc (Array2.kind a) sbsize n b in
  if up
  then geband_stub ~m:n ~n ~kl:0 ~ku:kd ~ar ~ac a ~br ~bc b
  else geband_stub ~m:n ~n ~kl:kd ~ku:0 ~ar ~ac a ~br ~bc b;
  (sbsize, n, br, bc, b)

let unsyband kd ?(up = true) ?(fill_num = None) ?a (sbsize, n, br, bc, b) =
  assert(sbsize = kd + 1);
  let ar, ac, a = opt_mat_alloc (Array2.kind b) n n a in
  if up
  then ungeband_stub ~m:n ~n ~kl:0 ~ku:kd ~fill_num ~ar ~ac a ~br ~bc b
  else ungeband_stub ~m:n ~n ~kl:kd ~ku:0 ~fill_num ~ar ~ac a ~br ~bc b;
  (n, n, ar, ac, a)

let luband_dyn kl ku ?ab (m, n, ar, ac, a) =
  if kl >= m then invalid_arg "Slap.Mat.geband_dyn: kl >= m";
  if ku >= n then invalid_arg "Slap.Mat.geband_dyn: ku >= n";
  let lusize = 2 * kl + ku + 1 in
  let abr, abc, ab = opt_mat_alloc (Array2.kind a) lusize n ab in
  geband_stub ~m ~n ~kl ~ku ~ar ~ac a ~br:(abr + kl) ~bc:abc ab;
  (lusize, n, abr, abc, ab)

let unluband m kl ku ?(fill_num = None) ?a (lusize, n, abr, abc, ab) =
  assert(lusize = 2 * kl + ku + 1);
  let ar, ac, a = opt_mat_alloc (Array2.kind ab) m n a in
  ungeband_stub ~m ~n ~kl ~ku ~fill_num ~ar ~ac a ~br:(abr + kl) ~bc:abc ab;
  (m, n, ar, ac, a)

(** {2 Iterators} *)

let mapi kind f ?b (m, n, ar, ac, a) =
  let br, bc, b = opt_mat_alloc kind m n b in
  for j = 0 to n - 1 do
    for i = 0 to m - 1 do
      let e = Array2.unsafe_get a (i + ar) (j + ac) in
      Array2.unsafe_set b (i + br) (j + bc) (f (i + 1) (j + 1) e)
    done
  done;
  (m, n, br, bc, b)

let map kind f = mapi kind (fun _ _ -> f)

let fold_lefti f init (m, n, ar, ac, a) =
  let v = reshape a in
  let lda = Array2.dim1 a in
  let ofs = (ac - 2) * lda + ar in
  let rec loop j acc =
    if j > n then acc else
      begin
        let colvec = (m, ofs + j * lda, 1, v) in
        loop (j + 1) (f j acc colvec)
      end
  in
  loop 1 init

let fold_left f = fold_lefti (fun _ -> f)

let fold_righti f (m, n, ar, ac, a) init =
  let v = reshape a in
  let lda = Array2.dim1 a in
  let ofs = (ac - 2) * lda + ar in
  let rec loop j acc =
    if j = 0 then acc else
      begin
        let colvec = (m, ofs + j * lda, 1, v) in
        loop (j - 1) (f j colvec acc)
      end
  in
  loop n init

let fold_right f = fold_righti (fun _ -> f)

let fold_topi f init (m, n, ar, ac, a) =
  let v = reshape a in
  let lda = Array2.dim1 a in
  let ofs = (ac - 1) * lda + ar - 1 in
  let rec loop i acc =
    if i > m then acc else
      begin
        let rowvec = (n, ofs + i, lda, v) in
        loop (i + 1) (f i acc rowvec)
      end
  in
  loop 1 init

let fold_top f = fold_topi (fun _ -> f)

let fold_bottomi f (m, n, ar, ac, a) init =
  let v = reshape a in
  let lda = Array2.dim1 a in
  let ofs = (ac - 1) * lda + ar - 1 in
  let rec loop i acc =
    if i = 0 then acc else
      begin
        let rowvec = (n, ofs + i, lda, v) in
        loop (i - 1) (f i rowvec acc)
      end
  in
  loop m init

let fold_bottom f = fold_bottomi (fun _ -> f)

let replace_all (m, n, ar, ac, a) f =
  for j = ac to n + ac - 1 do
    for i = ar to m + ar - 1 do
      Array2.unsafe_set a i j (f (Array2.unsafe_get a i j))
    done
  done

let replace_alli (m, n, ar, ac, a) f =
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
  assert(dim_array_array aa <> None);
  (m, n, 1, 1, Array2.of_array kind fortran_layout aa)

let of_array_dyn kind m n aa =
  match dim_array_array aa with
  | Some (m', n') when m = 0 || (m = m' && n = n') ->
     unsafe_of_array kind m n aa
  | _ -> invalid_arg "Slap.Mat.of_array_dyn"

let to_list (m, n, ar, ac, a) =
  let ar = ar - 1 in
  let ac = ac - 1 in
  let col_to_list i =
    Size.fold_righti (fun j l -> a.{i+ar, j+ac} :: l) n []
  in
  Size.fold_righti (fun i ll -> (col_to_list i) :: ll) m []

let unsafe_of_list kind m n ll =
  assert(dim_list_list ll <> None);
  let mat = create_bigarray kind m n in
  let list_iteri f l = ignore (List.fold_left (fun i x -> f i x; i + 1) 1 l) in
  list_iteri (fun i l -> list_iteri (fun j x -> mat.{i,j} <- x) l) ll;
  (m, n, 1, 1, mat)

let of_list_dyn kind m n ll =
  match dim_list_list ll with
  | Some (m', n') when m = 0 || (m = m' && n = n') ->
     unsafe_of_list kind m n ll
  | _ -> invalid_arg "Slap.Mat.of_list_dyn"

(** {2 Submatrices} *)

let submat_dyn m n ?(ar=1) ?(ac=1) (m', n', ar', ac', a) =
  if ar < 1 || ar > m' || m' < m + ar - 1 ||
     ac < 1 || ac > n' || n' < n + ac - 1
    then invalid_arg "Slap.Mat.submat_dyn" ;
  (m, n, ar + ar' - 1, ac + ac' - 1, a)
