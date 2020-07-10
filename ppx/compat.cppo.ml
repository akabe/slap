open Asttypes
open Parsetree

#if OCAML_VERSION >= (4, 03, 0)

(* OCaml 4.03.0 or above *)

let nolabel = Nolabel (* Asttypes.arg_label for 4.03 only *)

module Const =
struct
  let int ?suffix n = Pconst_integer (string_of_int n, suffix)
#if OCAML_VERSION >= (4, 11, 0)
  let string ~loc ?quot s = Pconst_string (s, loc, quot)
#else
  let string ~loc:_ ?quot s = Pconst_string (s, quot)
#endif
end

#else

(* OCaml 4.00.0 .. 4.02.3 *)

let nolabel = ""

module Const =
struct
  let int ?suffix n = Const_int n
  let string ~loc:_ ?quot s = Const_string (s, quot)
end

#endif
