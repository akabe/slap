open OUnit
open Bigarray
open Slap.Size
open Slap.Vec

let test_vec_float32 () =
  let f n l = of_list_dyn float32 n l in
  let (=) x y = Slap.S.Vec.ssqr_diff x y < 1e-6 in
  "v_emp" @? (f zero []               = [%vec.float32 []]);
  "v_sgl" @? (f one  [42.]            = [%vec.float32 [42.]]);
  "v_ord" @? (f five [1.;2.;3.;4.;5.] = [%vec.float32 [1.;2.;3.;4.;5.]])

let test_vec_float64 () =
  let f n l = of_list_dyn float64 n l in
  let (=) x y = Slap.D.Vec.ssqr_diff x y < 1e-6 in
  "v_emp" @? (f zero []               = [%vec.float64 []]);
  "v_sgl" @? (f one  [42.]            = [%vec.float64 [42.]]);
  "v_ord" @? (f five [1.;2.;3.;4.;5.] = [%vec.float64 [1.;2.;3.;4.;5.]])

let test_vec_complex32 () =
  let f n l = of_list_dyn complex32 n l in
  let (=) x y = Complex.norm (Slap.C.Vec.ssqr_diff x y) < 1e-6 in
  let c x = Complex.({re = x; im = x}) in
  "v_emp" @? (f zero []      = [%vec.complex32 []]);
  "v_sgl" @? (f one  [c 42.] = [%vec.complex32 [c 42.]]);
  "v_ord" @? (f five [c 1.;c 2.;c 3.;c 4.;c 5.]
              = [%vec.complex32 [c 1.;c 2.;c 3.;c 4.;c 5.]])

let test_vec_complex64 () =
  let f n l = of_list_dyn complex64 n l in
  let (=) x y = Complex.norm (Slap.Z.Vec.ssqr_diff x y) < 1e-6 in
  let c x = Complex.({re = x; im = x}) in
  "v_emp" @? (f zero []      = [%vec.complex64 []]);
  "v_sgl" @? (f one  [c 42.] = [%vec.complex64 [c 42.]]);
  "v_ord" @? (f five [c 1.;c 2.;c 3.;c 4.;c 5.]
              = [%vec.complex64 [c 1.;c 2.;c 3.;c 4.;c 5.]])

let test_vec_char () =
  let f n l = of_list_dyn char n l in
  "v_emp" @? (f zero []                    = [%vec.char []]);
  "v_sgl" @? (f one  ['x']                 = [%vec.char ['x']]);
  "v_ord" @? (f five ['a';'b';'c';'d';'e'] = [%vec.char ['a';'b';'c';'d';'e']])

let test_vec_sint8 () =
  let f n l = of_list_dyn int8_signed n l in
  "v_emp" @? (f zero []          = [%vec.sint8 []]);
  "v_sgl" @? (f one  [42]        = [%vec.sint8 [42]]);
  "v_ord" @? (f five [1;2;3;4;5] = [%vec.sint8 [1;2;3;4;5]])

let test_vec_uint8 () =
  let f n l = of_list_dyn int8_unsigned n l in
  "v_emp" @? (f zero []          = [%vec.uint8 []]);
  "v_sgl" @? (f one  [42]        = [%vec.uint8 [42]]);
  "v_ord" @? (f five [1;2;3;4;5] = [%vec.uint8 [1;2;3;4;5]])

let test_vec_sint16 () =
  let f n l = of_list_dyn int16_signed n l in
  "v_emp" @? (f zero []          = [%vec.sint16 []]);
  "v_sgl" @? (f one  [42]        = [%vec.sint16 [42]]);
  "v_ord" @? (f five [1;2;3;4;5] = [%vec.sint16 [1;2;3;4;5]])

let test_vec_uint16 () =
  let f n l = of_list_dyn int16_unsigned n l in
  "v_emp" @? (f zero []          = [%vec.uint16 []]);
  "v_sgl" @? (f one  [42]        = [%vec.uint16 [42]]);
  "v_ord" @? (f five [1;2;3;4;5] = [%vec.uint16 [1;2;3;4;5]])

let test_vec_int () =
  let f n l = of_list_dyn int n l in
  "v_emp" @? (f zero []          = [%vec.int []]);
  "v_sgl" @? (f one  [42]        = [%vec.int [42]]);
  "v_ord" @? (f five [1;2;3;4;5] = [%vec.int [1;2;3;4;5]])

let test_vec_int32 () =
  let d = Int32.of_int in
  let f n l = of_list_dyn int32 n l in
  "v_emp" @? (f zero []                    = [%vec.int32 []]);
  "v_sgl" @? (f one  [d 42]                = [%vec.int32 [d 42]]);
  "v_ord" @? (f five [d 1;d 2;d 3;d 4;d 5] = [%vec.int32 [d 1;d 2;d 3;d 4;d 5]])

let test_vec_int64 () =
  let d = Int64.of_int in
  let f n l = of_list_dyn int64 n l in
  "v_emp" @? (f zero []                    = [%vec.int64 []]);
  "v_sgl" @? (f one  [d 42]                = [%vec.int64 [d 42]]);
  "v_ord" @? (f five [d 1;d 2;d 3;d 4;d 5] = [%vec.int64 [d 1;d 2;d 3;d 4;d 5]])

let suite =
  "%vec" >::: [
    "float32" >:: test_vec_float32;
    "float64" >:: test_vec_float64;
    "complex32" >:: test_vec_complex32;
    "complex64" >:: test_vec_complex64;
    "char" >:: test_vec_char;
    "sint8" >:: test_vec_sint8;
    "uint8" >:: test_vec_uint8;
    "sint16" >:: test_vec_sint16;
    "uint16" >:: test_vec_uint16;
    "int" >:: test_vec_int;
    "int32" >:: test_vec_int32;
    "int64" >:: test_vec_int64;
  ]
