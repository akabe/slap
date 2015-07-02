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

open Format
open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident

type kind =
  | Default
  | Default_real
  | Float32
  | Float64
  | Int8_signed
  | Int8_unsigned
  | Int16_signed
  | Int16_unsigned
  | Int32
  | Int64
  | Int
  | Nativeint
  | Complex32
  | Complex64
  | Char

type ext =
  | Vec of kind
  | Mat of kind
  | Unknown

exception Error of Location.t * string

let errorf loc fmt = ksprintf (fun s () -> raise (Error (loc, s))) fmt

let kind_of_string ~loc = function
  | "" -> Default
  | "float32" | "S" | "s" -> Float32
  | "float64" | "D" | "d" -> Float64
  | "complex32" | "C" | "c" -> Complex32
  | "complex64" | "Z" | "z" -> Complex64
  | "int8_signed" | "sint8" -> Int8_signed
  | "int8_unsigned" | "uint8" -> Int8_unsigned
  | "int16_signed" | "sint16" -> Int16_signed
  | "int16_unsigned" | "uint16" -> Int16_unsigned
  | "int" -> Int
  | "int32" -> Int32
  | "int64" -> Int64
  | "nativeint" -> Nativeint
  | "char" -> Char
  | s -> errorf loc "Error: @[Unknown kind `%s'@]" s ()

let sep_str s c =
  try
    let i = String.index s c in
    let s1 = String.sub s 0 i in
    let s2 = String.sub s (i + 1) (String.length s - i - 1) in
    (s1, s2)
  with
  | Not_found -> (s, "")

let ext_of_string ~loc s =
  match sep_str s '.' with
  | ("vec", s2) -> Vec (kind_of_string ~loc s2)
  | ("mat", s2) -> Mat (kind_of_string ~loc s2)
  | ("rvec", "") -> Vec Default_real
  | ("rmat", "") -> Mat Default_real
  | _ -> Unknown

module Li = (* Longident.t loc *)
struct
  let mk ?(loc = !default_loc) name = { txt = parse name; loc = loc; }
end

module Typ =
struct
  include Ast_helper.Typ

  let constr ?loc ?attrs name args = constr ?loc ?attrs (Li.mk ?loc name) args

  (* for big arrays *)

  let bigarray_ocaml_elt ?loc ?attrs kind =
    let name = match kind with
      | Default -> "num_type"
      | Default_real -> "float"
      | Float32 | Float64 -> "float"
      | Int8_signed | Int8_unsigned | Int16_signed | Int16_unsigned
      | Int -> "int"
      | Int32 -> "int32"
      | Int64 -> "int64"
      | Nativeint -> "nativeint"
      | Complex32 | Complex64 -> "Complex.t"
      | Char -> "char" in
    constr ?loc ?attrs name []

  (* for SLAP *)

  let size ?loc ?attrs n =
    let typ_of_size base n0 n =
      let rec loop t i =
        if i = n then t
        else loop (constr ?loc ?attrs "Slap.Size.s" [t]) (i + 1)
      in
      loop (constr ?loc ?attrs base []) n0
    in
    let t = match n with
      | 0 -> constr ?loc ?attrs "Slap.Size.z" []
      | 1 -> constr ?loc ?attrs "Slap.Size.one" []
      | 2 -> constr ?loc ?attrs "Slap.Size.two" []
      | 3 -> constr ?loc ?attrs "Slap.Size.three" []
      | 4 -> constr ?loc ?attrs "Slap.Size.four" []
      | 5 -> constr ?loc ?attrs "Slap.Size.five" []
      | 6 -> constr ?loc ?attrs "Slap.Size.six" []
      | 7 -> constr ?loc ?attrs "Slap.Size.seven" []
      | 8 -> constr ?loc ?attrs "Slap.Size.eight" []
      | 9 -> constr ?loc ?attrs "Slap.Size.nine" []
      | 10 -> constr ?loc ?attrs "Slap.Size.ten" []
      | n -> typ_of_size "Slap.Size.ten" 10 n
    in
    constr ?loc ?attrs "Slap.Size.t" [t]

  let dummy_args ?loc ?attrs n =
    let rec loop acc i =
      if i = 0 then acc else loop ((any ?loc ?attrs ()) :: acc) (i - 1)
    in
    loop [] n

  let vec ?loc ?attrs kind =
    let name, n = match kind with
      | Default -> "vec", 2
      | Default_real -> "rvec", 2
      | Float32 -> "Slap.S.vec", 2
      | Float64 -> "Slap.D.vec", 2
      | Complex32 -> "Slap.C.vec", 2
      | Complex64 -> "Slap.Z.vec", 2
      | _ -> "Slap.Vec.t", 4 in
    constr ?loc ?attrs name (dummy_args n)

  let mat ?loc ?attrs kind =
    let name, n = match kind with
      | Default -> "mat", 3
      | Float32 -> "Slap.S.mat", 3
      | Float64 -> "Slap.D.mat", 3
      | Complex32 -> "Slap.C.mat", 3
      | Complex64 -> "Slap.Z.mat", 3
      | _ -> "Slap.Mat.t", 5 in
    constr ?loc ?attrs name (dummy_args n)
end

module Pat =
struct
  include Ast_helper.Pat

  let var ?(loc = !default_loc) ?attrs name =
    var ~loc ?attrs { txt = name; loc = loc }
end

module Exp =
struct
  include Ast_helper.Exp

  let ident ?loc ?attrs name = ident ?loc ?attrs (Li.mk name)
  let int ?loc ?attrs n = constant ?loc ?attrs (Const_int n)

  let rev_sequence_list ?loc ?attrs ~last el =
    List.fold_left (fun acc x -> sequence ?loc ?attrs x acc) last el

  (* for big arrays *)

  let bigarray_kind ?loc ?attrs kind =
    let name = match kind with
      | Default -> "prec"
      | Default_real -> "rprec"
      | Float32 -> "Bigarray.float32"
      | Float64 -> "Bigarray.float64"
      | Int8_signed -> "Bigarray.int8_signed"
      | Int8_unsigned -> "Bigarray.int8_unsigned"
      | Int16_signed -> "Bigarray.int16_signed"
      | Int16_unsigned -> "Bigarray.int16_unsigned"
      | Int -> "Bigarray.int"
      | Int32 -> "Bigarray.int32"
      | Int64 -> "Bigarray.int64"
      | Nativeint -> "Bigarray.nativeint"
      | Complex32 -> "Bigarray.complex32"
      | Complex64 -> "Bigarray.complex64"
      | Char -> "Bigarray.char" in
    ident ?loc ?attrs name

  let bigarray_layout ?loc ?attrs () =
    ident ?loc ?attrs "Bigarray.fortran_layout"

  let bigarray_create ?loc ?attrs kind dims =
    let fun_name = "Bigarray.Array"
                   ^ (string_of_int (List.length dims))
                   ^ ".create" in
    let args = ("", bigarray_kind ?loc ?attrs kind)
               :: ("", bigarray_layout  ?loc ?attrs ())
               :: List.map (fun n -> ("", int ?loc ?attrs n)) dims in
    apply ?loc ?attrs (ident ?loc ?attrs fun_name) args

  let bigarray_unsafe_set ?loc ?attrs e_ba indices e_elm =
    let fun_name = "Bigarray.Array"
                   ^ (string_of_int (List.length indices))
                   ^ ".unsafe_set" in
    let args = ("", e_ba)
               :: List.map (fun i -> ("", int ?loc ?attrs i)) indices
               @ [("", e_elm)] in
    apply ?loc ?attrs (ident ?loc ?attrs fun_name) args

  let bigarray =
    let c = ref 0 in
    let gen_name () = incr c; "__slap_ba$" ^ (string_of_int !c) in
    let bigarray_aux ?loc ?attrs kind dims items =
      let var_name = gen_name () in
      let e_var = ident ?loc ?attrs var_name in
      let t_elt = Typ.bigarray_ocaml_elt ?loc ?attrs kind in
      let mk_unsafe_set (indices, e) =
        let e' = Exp.constraint_ ?loc ?attrs e t_elt in
        bigarray_unsafe_set ?loc ?attrs e_var indices e'
      in
      let e_create_ba = bigarray_create ?loc ?attrs kind dims in
      List.map mk_unsafe_set items
      |> rev_sequence_list ?loc ?attrs ~last:e_var
      |> let_ ?loc ?attrs Nonrecursive [Vb.mk (Pat.var var_name) e_create_ba]
    in
    bigarray_aux

  (* for SLAP *)

  let size n =
    let e_size = apply (ident "Slap.Size.__unexpose")
        ["", constant (Const_int n)] in
    constraint_ e_size (Typ.size n)

  let vec kind el =
    let n = List.length el in
    let items = List.mapi (fun i e -> ([i+1], e)) el in
    let e_ba = bigarray kind [n] items in
    let e_tuple = tuple [size n; (* dimension *)
                         constant (Const_int 1); (* offset *)
                         constant (Const_int 1); (* incrementation *)
                         e_ba] in
    let e_vec = apply (ident "Slap.Vec.__unexpose") ["", e_tuple] in
    constraint_ e_vec (Typ.vec kind)

  let mat kind ell =
    let m, n = match ell with
      | [] -> (0, 0)
      | el :: rest -> (List.length rest + 1, List.length el) in
    let items = List.mapi (fun i -> List.mapi (fun j e -> ([i+1; j+1], e))) ell
                |> List.flatten in
    let e_ba = bigarray kind [m; n] items in
    let e_tuple = tuple [size m; (* dimension (#rows) *)
                         size n; (* dimension (#columns) *)
                         constant (Const_int 1); (* offset of rows *)
                         constant (Const_int 1); (* offset of columns *)
                         e_ba] in
    let e_vec = apply (ident "Slap.Mat.__unexpose") ["", e_tuple] in
    constraint_ e_vec (Typ.mat kind)

  (* For errors *)

  (* error report: [%ocaml.error "message"] *)
  let error ?(loc = !default_loc) ?attrs msg =
    let e_str = Exp.constant ~loc ?attrs (Const_string (msg, None)) in
    Exp.extension ({ txt = "ocaml.error"; loc },
                   PStr [Str.eval ~loc ?attrs e_str])
end

let get_exprs_list e0 =
  let rec list acc = function
    | { pexp_desc = Pexp_construct ({ txt = Lident "[]" }, None) } ->
      List.rev acc
    | { pexp_desc = Pexp_construct
            ({ txt = Lident "::" },
             Some ({ pexp_desc = Pexp_tuple ([elm; rest]) })) } ->
      list (elm :: acc) rest
    | e ->
      errorf e.pexp_loc
        "Syntax Error: @[This expression should be a list syntactically@]" ()
  in
  list [] e0

let get_exprs_list_or_tuple = function
  | { pexp_desc = Pexp_tuple el } -> el
  | e0 -> try get_exprs_list e0 with Error _ -> [e0]

let get_exprs_matrix e =
  let get_column acc ei =
    let eil = get_exprs_list_or_tuple ei in
    let n' = List.length eil in
    match acc with
    | None -> Some (n', [eil])
    | Some (n, acc) ->
      if n = n' then Some (n, eil :: acc)
      else errorf ei.pexp_loc
          "Error: @[The length of this column is %d,@ \
           but it should be %d@]" n' n ()
  in
  match List.fold_left get_column None (get_exprs_list e) with
  | None -> []
  | Some (_, ell) -> List.rev ell

let transform_vec kind e =
  let el = get_exprs_list e in
  Exp.vec kind el

let transform_mat kind e =
  let ell = get_exprs_matrix e in
  Exp.mat kind ell

let transform loc transformer kind payload =
  match payload with
  | PStr [{ pstr_desc = Pstr_eval (e, _) }] ->
    let old_loc = !default_loc in
    default_loc := e.pexp_loc;
    let e' = transformer kind e in
    default_loc := old_loc;
    e'
  | _ ->
    errorf loc "Syntax Error: \
                @[This expression should be a list syntaxtically@]" ()

let slap_mapper =
  let super = default_mapper in
  let expr self e = match e with
    | { pexp_desc = Pexp_extension ({txt}, payload) } ->
      let loc = e.pexp_loc in
      begin
        try
          match ext_of_string ~loc txt with
          | Vec kind -> transform loc transform_vec kind payload
          | Mat kind -> transform loc transform_mat kind payload
          | Unknown -> super.expr self e
        with
        | Error (loc, msg) -> Exp.error ~loc msg
      end
    | _ -> super.expr self e
  in
  { super with expr }

let () =
  match Sys.argv with
  | [|_; infile; outfile|] ->
    Ast_mapper.apply ~source:infile ~target:outfile slap_mapper
  | _ ->
    eprintf "ppx_slap infile outfile"
