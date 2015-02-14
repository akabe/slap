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
open Slap_common

type (+'m, +'n, 'num, 'prec, +'cnt_or_dsc) t =
  ('m, 'n, 'num, 'prec, 'cnt_or_dsc) mat

let cnt a = __unexpose_mat (__expose_mat a)

let check_cnt a =
  let m, n, ar, ac, a = __expose_mat a in
  __expose_size m = Array2.dim1 a
  && __expose_size n = Array2.dim2 a
  && ar = 1
  && ac = 1

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

external create_array2 :
  ('a, 'b) kind -> 'm size -> 'n size -> ('a, 'b, fortran_layout) Array2.t
  = "slap_mat_create_array2"

let opt_mat m n = function
  | None -> (None, None, None)
  | Some a ->
    let m', n', ar, ac, a = __expose_mat a in
    assert(m = m' && n = n');
    (Some ar, Some ac, Some a)

let opt_mat_alloc kind m n = function
  | None ->
     (1, 1, create_array2 kind m n)
  | Some a ->
    let m', n', ar, ac, a = __expose_mat a in
    assert(m = m' && n = n');
    (ar, ac, a)

(** {2 Creation of matrices} *)

let create kind m n =
  __unexpose_mat (m, n, 1, 1, create_array2 kind m n)

let make kind m n x =
  let a = create_array2 kind m n in
  Array2.fill a x;
  __unexpose_mat (m, n, 1, 1, a)

let init_cols kind m n f =
  let a = create_array2 kind m n in
  for j = 1 to __expose_size n do
    for i = 1 to __expose_size m do
      Array2.unsafe_set a i j (f i j)
    done
  done;
  __unexpose_mat (m, n, 1, 1, a)

let init_rows kind m n f =
  let a = create_array2 kind m n in
  for i = 1 to __expose_size m do
    for j = 1 to __expose_size n do
      Array2.unsafe_set a i j (f i j)
    done
  done;
  __unexpose_mat (m, n, 1, 1, a)

let init = init_cols

(** {2 Accessors} *)

let kind ma =
  let _, _, _, _, a = __expose_mat ma in
  Array2.kind a

let dim ma =
  let m, n, _, _, _ = __expose_mat ma in
  (m, n)

let dim1 ma =
  let m, _, _, _, _ = __expose_mat ma in
  m

let dim2 ma =
  let _, n, _, _, _ = __expose_mat ma in
  n

let check_bounds m n i j loc =
  if 1 > i || i > __expose_size m || 1 > j || j > __expose_size n
  then invalid_arg loc

let get_dyn ma i j =
  let m, n, ar, ac, a = __expose_mat ma in
  check_bounds m n i j "Slap.Mat.get_dyn";
  Array2.get a (i + ar - 1) (j + ac - 1)

let set_dyn ma i j x =
  let m, n, ar, ac, a = __expose_mat ma in
  check_bounds m n i j "Slap.Mat.set_dyn";
  Array2.set a (i + ar - 1) (j + ac - 1) x

let unsafe_get ma i j =
  let _, _, ar, ac, a = __expose_mat ma in
  Array2.unsafe_get a (i + ar - 1) (j + ac - 1)

let unsafe_set ma i j x =
  let _, _, ar, ac, a = __expose_mat ma in
  Bigarray.Array2.unsafe_set a (i + ar - 1) (j + ac - 1) x

let replace_dyn ma i j f =
  let m, n, ar, ac, a = __expose_mat ma in
  check_bounds m n i j "Slap.Mat.replace_dyn";
  let i = i + ar - 1 in
  let j = j + ac - 1 in
  Array2.unsafe_set a i j (f (Array2.unsafe_get a i j))

let reshape a =
  let m = Array2.dim1 a in
  let n = Array2.dim2 a in
  reshape_1 (genarray_of_array2 a) (m * n)

let col_dyn ma i =
  let m, n, ar, ac, a = __expose_mat ma in
  if i < 1 || i > __expose_size n then invalid_arg "Slap.Mat.col_dyn";
  let lda = Array2.dim1 a in
  __unexpose_vec (m, lda * (i + ac - 2) + ar, 1, reshape a)

let row_dyn ma i =
  let m, n, ar, ac, a = __expose_mat ma in
  if i < 1 || i > __expose_size m then invalid_arg "Slap.Mat.row_dyn";
  let lda = Array2.dim1 a in
  __unexpose_vec (n, lda * (ac - 1) + ar + i - 1, lda, reshape a)

let diag a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  let lda = Array2.dim1 a in
  __unexpose_vec (n, lda * (ac - 1) + ar, lda + 1, reshape a)

let diag_rect a =
  let m, n, ar, ac, a = __expose_mat a in
  let lda = Array2.dim1 a in
  __unexpose_vec (Slap_size.min m n, lda * (ac - 1) + ar, lda + 1, reshape a)

let as_vec a =
  assert(check_cnt a);
  let m, n, _, _, a = __expose_mat a in
  __unexpose_vec (Slap_size.mul m n, 1, 1, reshape a)

(** {2 Basic operations} *)

external fill_stub : m:'m size -> n:'n size ->
                     ar:int -> ac:int ->
                     ('num, 'prec, fortran_layout) Array2.t ->
                     'num -> unit
  = "slap_mat_fill_stub_bc" "slap_mat_fill_stub"

let fill ma x =
  let m, n, ar, ac, a = __expose_mat ma in
  fill_stub ~m ~n ~ar ~ac a x

external copy_stub : m:'m size -> n:'n size ->
                     ar:int -> ac:int ->
                     ('num, 'prec, fortran_layout) Array2.t ->
                     br:int -> bc:int ->
                     ('num, 'prec, fortran_layout) Array2.t -> unit
  = "slap_mat_copy_stub_bc" "slap_mat_copy_stub"

let copy ?b ma =
  let m, n, ar, ac, a = __expose_mat ma in
  let br, bc, b = opt_mat_alloc (Array2.kind a) m n b in
  copy_stub ~m ~n ~ar ~ac a ~br ~bc b;
  __unexpose_mat (m, n, br, bc, b)

(** {2 Matrix transformations} *)

external packed_stub : n:'n size -> up:bool ->
                       ('num, 'prec, fortran_layout) Array1.t ->
                       ar:int -> ac:int ->
                       ('num, 'prec, fortran_layout) Array2.t -> unit
  = "slap_mat_packed_stub_bc" "slap_mat_packed_stub"

let packed ?(up = true) ?x ma =
  let n, n', ar, ac, a = __expose_mat ma in
  assert(n = n');
  let k = Slap_size.packed n in
  let x = Slap_vec.opt_cnt_vec_alloc (Array2.kind a) k x in
  packed_stub ~up ~n x ~ar ~ac a;
  __unexpose_vec (k, 1, 1, x)

external unpacked_stub : n:'n size -> up:bool ->
                         fill_num:'num option ->
                         ('num, 'prec, fortran_layout) Array1.t ->
                         ar:int -> ac:int ->
                         ('num, 'prec, fortran_layout) Array2.t -> unit
  = "slap_mat_unpacked_stub_bc" "slap_mat_unpacked_stub"

let unpacked ?(up = true) ?(fill_num = None) ?a x =
  assert(Slap_vec.check_cnt x);
  let k, _, _, x = __expose_vec x in
  let n = Slap_size.unpacked k in
  let ar, ac, a = opt_mat_alloc (Array1.kind x) n n a in
  unpacked_stub ~n ~up ~fill_num x ~ar ~ac a;
  __unexpose_mat (n, n, ar, ac, a)

external geband_stub : m:'m size -> n:'n size ->
                       kl:'kl size -> ku:'ku size ->
                       ar:int -> ac:int ->
                       ('num, 'prec, fortran_layout) Array2.t ->
                       br:int -> bc:int ->
                       ('num, 'prec, fortran_layout) Array2.t -> unit
  = "slap_mat_geband_stub_bc" "slap_mat_geband_stub"

let geband_dyn kl ku ?b ma =
  let m, n, ar, ac, a = __expose_mat ma in
  let gbsize = Slap_size.geband_dyn m n kl ku in
  let br, bc, b = opt_mat_alloc (Array2.kind a) gbsize n b in
  geband_stub ~m ~n ~kl ~ku ~ar ~ac a ~br ~bc b;
  __unexpose_mat (gbsize, n, br, bc, b)

external ungeband_stub : m:'m size -> n:'n size ->
                         kl:'kl size -> ku:'ku size ->
                         fill_num:'num option ->
                         ar:int -> ac:int ->
                         ('num, 'prec, fortran_layout) Array2.t ->
                         br:int -> bc:int ->
                         ('num, 'prec, fortran_layout) Array2.t -> unit
  = "slap_mat_ungeband_stub_bc" "slap_mat_ungeband_stub"

let ungeband m kl ku ?(fill_num = None) ?a b =
  let gbsize, n, br, bc, b = __expose_mat b in
  assert(gbsize = Slap_size.geband_dyn m n kl ku);
  let ar, ac, a = opt_mat_alloc (Array2.kind b) m n a in
  ungeband_stub ~m ~n ~kl ~ku ~fill_num ~ar ~ac a ~br ~bc b;
  __unexpose_mat (m, n, ar, ac, a)

let syband_dyn kd ?(up = true) ?b a =
  let n, n', ar, ac, a = __expose_mat a in
  assert(n = n');
  let sbsize = Slap_size.syband_dyn n kd in
  let br, bc, b = opt_mat_alloc (Array2.kind a) sbsize n b in
  if up
  then geband_stub ~m:n ~n ~kl:Slap_size.zero ~ku:kd ~ar ~ac a ~br ~bc b
  else geband_stub ~m:n ~n ~kl:kd ~ku:Slap_size.zero ~ar ~ac a ~br ~bc b;
  __unexpose_mat (sbsize, n, br, bc, b)

let unsyband kd ?(up = true) ?(fill_num = None) ?a b =
  let sbsize, n, br, bc, b = __expose_mat b in
  assert(sbsize = Slap_size.syband_dyn n kd);
  let ar, ac, a = opt_mat_alloc (Array2.kind b) n n a in
  if up
  then ungeband_stub ~m:n ~n ~kl:Slap_size.zero ~ku:kd
      ~fill_num ~ar ~ac a ~br ~bc b
  else ungeband_stub ~m:n ~n ~kl:kd ~ku:Slap_size.zero
      ~fill_num ~ar ~ac a ~br ~bc b;
  __unexpose_mat (n, n, ar, ac, a)

let luband_dyn kl ku ?ab a =
  let m, n, ar, ac, a = __expose_mat a in
  let lusize = Slap_size.luband_dyn m n kl ku in
  let abr, abc, ab = opt_mat_alloc (Array2.kind a) lusize n ab in
  geband_stub ~m ~n ~kl ~ku ~ar ~ac a ~br:(abr + __expose_size kl) ~bc:abc ab;
  __unexpose_mat (lusize, n, abr, abc, ab)

let unluband m kl ku ?(fill_num = None) ?a ab =
  let lusize, n, abr, abc, ab = __expose_mat ab in
  assert(lusize = Slap_size.luband_dyn m n kl ku);
  let ar, ac, a = opt_mat_alloc (Array2.kind ab) m n a in
  ungeband_stub ~m ~n ~kl ~ku ~fill_num ~ar ~ac a
    ~br:(abr + __expose_size kl) ~bc:abc ab;
  __unexpose_mat (m, n, ar, ac, a)

(** {2 Iterators} *)

let mapi kind f ?b a =
  let m, n, ar, ac, a = __expose_mat a in
  let (br, bc, b) = opt_mat_alloc kind m n b in
  for j = 0 to __expose_size n - 1 do
    for i = 0 to __expose_size m - 1 do
      let e = Array2.unsafe_get a (i + ar) (j + ac) in
      Array2.unsafe_set b (i + br) (j + bc) (f (i + 1) (j + 1) e)
    done
  done;
  __unexpose_mat (m, n, br, bc, b)

let map kind f = mapi kind (fun _ _ -> f)

let fold_lefti f init a =
  let m, n, ar, ac, a = __expose_mat a in
  let v = reshape a in
  let lda = Array2.dim1 a in
  let ofs = (ac - 2) * lda + ar in
  let rec loop j acc =
    if j > __expose_size n then acc else
      begin
        let colvec = __unexpose_vec (m, ofs + j * lda, 1, v) in
        loop (j + 1) (f j acc colvec)
      end
  in
  loop 1 init

let fold_left f = fold_lefti (fun _ -> f)

let fold_righti f a init =
  let m, n, ar, ac, a = __expose_mat a in
  let v = reshape a in
  let lda = Array2.dim1 a in
  let ofs = (ac - 2) * lda + ar in
  let rec loop j acc =
    if j = 0 then acc else
      begin
        let colvec = __unexpose_vec (m, ofs + j * lda, 1, v) in
        loop (j - 1) (f j colvec acc)
      end
  in
  loop (__expose_size n) init

let fold_right f = fold_righti (fun _ -> f)

let fold_topi f init a =
  let m, n, ar, ac, a = __expose_mat a in
  let v = reshape a in
  let lda = Array2.dim1 a in
  let ofs = (ac - 1) * lda + ar - 1 in
  let rec loop i acc =
    if i > __expose_size m then acc else
      begin
        let rowvec = __unexpose_vec (n, ofs + i, lda, v) in
        loop (i + 1) (f i acc rowvec)
      end
  in
  loop 1 init

let fold_top f = fold_topi (fun _ -> f)

let fold_bottomi f a init =
  let m, n, ar, ac, a = __expose_mat a in
  let v = reshape a in
  let lda = Array2.dim1 a in
  let ofs = (ac - 1) * lda + ar - 1 in
  let rec loop i acc =
    if i = 0 then acc else
      begin
        let rowvec = __unexpose_vec (n, ofs + i, lda, v) in
        loop (i - 1) (f i rowvec acc)
      end
  in
  loop (__expose_size m) init

let fold_bottom f a init = fold_bottomi (fun _ -> f) a init


let replace_all a f =
  let m, n, ar, ac, a = __expose_mat a in
  for j = ac to __expose_size n + ac - 1 do
    for i = ar to __expose_size m + ar - 1 do
      Array2.unsafe_set a i j (f (Array2.unsafe_get a i j))
    done
  done

let replace_alli a f =
  let m, n, ar, ac, a = __expose_mat a in
  let ar = ar - 1 in
  let ac = ac - 1 in
  for j = 1 to __expose_size n do
    let y = j + ac in
    for i = 1 to __expose_size m do
      let x = i + ar in
      let e = Array2.unsafe_get a x y in
      Array2.unsafe_set a x y (f x y e)
    done
  done

(** {2 Type conversion} *)

let to_array a =
  let m, n, ar, ac, a = __expose_mat a in
  Array.init (__expose_size m)
    (fun i -> Array.init (__expose_size n) (fun j -> a.{i+ar,j+ac}))

let unsafe_of_array kind m n aa =
  assert(dim_array_array aa <> None);
  __unexpose_mat (m, n, 1, 1, Array2.of_array kind fortran_layout aa)

let of_array_dyn kind m n aa =
  match dim_array_array aa with
  | Some (m', n') when Slap_size.iszero m
                    || (__expose_size m = m' && __expose_size n = n') ->
     unsafe_of_array kind m n aa
  | _ -> invalid_arg "Slap.Mat.of_array_dyn"

let to_list a =
  let m, n, ar, ac, a = __expose_mat a in
  let ar = ar - 1 in
  let ac = ac - 1 in
  let col_to_list i =
    Slap_size.fold_righti (fun j l -> a.{i+ar, j+ac} :: l) n []
  in
  Slap_size.fold_righti (fun i ll -> (col_to_list i) :: ll) m []

let unsafe_of_list kind m n ll =
  assert(dim_list_list ll <> None);
  let mat = create_array2 kind m n in
  let list_iteri f l = ignore (List.fold_left (fun i x -> f i x; i + 1) 1 l) in
  list_iteri (fun i l -> list_iteri (fun j x -> mat.{i,j} <- x) l) ll;
  __unexpose_mat (m, n, 1, 1, mat)

let of_list_dyn kind m n ll =
  match dim_list_list ll with
  | Some (m', n') when __expose_size m = 0
                    || (__expose_size m = m' && __expose_size n = n') ->
     unsafe_of_list kind m n ll
  | _ -> invalid_arg "Slap.Mat.of_list_dyn"

(** {2 Submatrices} *)

let submat_dyn m n ?(ar=1) ?(ac=1) a =
  let m', n', ar', ac', a = __expose_mat a in
  let m' = __expose_size m' in
  let n' = __expose_size n' in
  if ar < 1 || ar > m' || m' < __expose_size m + ar - 1 ||
     ac < 1 || ac > n' || n' < __expose_size n + ac - 1
    then invalid_arg "Slap.Mat.submat_dyn" ;
  __unexpose_mat (m, n, ar + ar' - 1, ac + ac' - 1, a)
