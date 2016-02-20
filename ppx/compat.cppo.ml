open Asttypes
open Parsetree

#if OCAML_VERSION >= (4, 03, 0)

(* OCaml 4.03.0 or above *)

let nolabel = Nolabel (* Asttypes.arg_label for 4.03 only *)

module Const =
struct
  let int ?suffix n = PConst_int (string_of_int n, suffix)
  let string ?quot s = PConst_string (s, quot)
end

#else

(* OCaml 4.00.0 .. 4.02.3 *)

let nolabel = ""

module Const =
struct
  let int ?suffix n = Const_int n
  let string ?quot s = Const_string (s, quot)
end

#endif
