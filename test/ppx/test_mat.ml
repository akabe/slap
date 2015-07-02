open OUnit
open Bigarray
open Slap.Size
open Slap.Mat

let mat_eq sub lange x y =
  let (m, n) = dim x in
  match (to_int m, to_int n) with
    | (0, _) | (_, 0) -> true (* `dim x = dim y' is verified by type checker. *)
    | _ -> sub x y ; lange ?norm:None ?work:None y < 1e-6

let test_mat_float32 () =
  let f m n ll = of_list_dyn float32 m n ll in
  let (=) x y = Slap.S.(mat_eq (Mat.axpy ~alpha:(-1.)) lange x y) in
  "m_emp" @? (f zero zero [] = [%mat.float32 []]);
  "m_sgl" @? (f one  one  [[42.]] = [%mat.float32 [42.]]);
  "m_olp" @? (f two three [[11.;12.;13.]; [21.;22.;23.]]
              = [%mat.float32 [11.,12.,13.; 21.,22.,23.]]);
  "m_oll" @? (f two three [[11.;12.;13.]; [21.;22.;23.]]
              = [%mat.float32 [[11.;12.;13.]; [21.;22.;23.]]])

let test_mat_float64 () =
  let f m n ll = of_list_dyn float64 m n ll in
  let (=) x y = Slap.D.(mat_eq (Mat.axpy ~alpha:(-1.)) lange x y) in
  "m_emp" @? (f zero zero [] = [%mat.float64 []]);
  "m_sgl" @? (f one  one  [[42.]] = [%mat.float64 [42.]]);
  "m_olp" @? (f two three [[11.;12.;13.]; [21.;22.;23.]]
              = [%mat.float64 [11.,12.,13.; 21.,22.,23.]]);
  "m_oll" @? (f two three [[11.;12.;13.]; [21.;22.;23.]]
              = [%mat.float64 [[11.;12.;13.]; [21.;22.;23.]]])

let test_mat_complex32 () =
  let f m n ll = of_list_dyn complex32 m n ll in
  let alpha = Complex.({re = (-1.); im = 0.}) in
  let c x = Complex.({re = x; im = x}) in
  let (=) x y = Slap.C.(mat_eq (Mat.axpy ~alpha) lange x y) in
  "m_emp" @? (f zero zero [] = [%mat.complex32 []]);
  "m_sgl" @? (f one  one  [[c 42.]] = [%mat.complex32 [c 42.]]);
  "m_olp" @? (f two three [[c 11.;c 12.;c 13.]; [c 21.;c 22.;c 23.]]
              = [%mat.complex32 [c 11.,c 12.,c 13.; c 21.,c 22.,c 23.]]);
  "m_oll" @? (f two three [[c 11.;c 12.;c 13.]; [c 21.;c 22.;c 23.]]
              = [%mat.complex32 [[c 11.;c 12.;c 13.]; [c 21.;c 22.;c 23.]]])

let test_mat_complex64 () =
  let f m n ll = of_list_dyn complex64 m n ll in
  let alpha = Complex.({re = (-1.); im = 0.}) in
  let c x = Complex.({re = x; im = x}) in
  let (=) x y = Slap.Z.(mat_eq (Mat.axpy ~alpha) lange x y) in
  "m_emp" @? (f zero zero [] = [%mat.complex64 []]);
  "m_sgl" @? (f one  one  [[c 42.]] = [%mat.complex64 [c 42.]]);
  "m_olp" @? (f two three [[c 11.;c 12.;c 13.]; [c 21.;c 22.;c 23.]]
              = [%mat.complex64 [c 11.,c 12.,c 13.; c 21.,c 22.,c 23.]]);
  "m_oll" @? (f two three [[c 11.;c 12.;c 13.]; [c 21.;c 22.;c 23.]]
              = [%mat.complex64 [[c 11.;c 12.;c 13.]; [c 21.;c 22.;c 23.]]])

let test_mat_char () =
  let f m n ll = of_list_dyn char m n ll in
  "m_emp" @? (f zero zero [] = [%mat.char []]);
  "m_sgl" @? (f one  one  [['x']] = [%mat.char ['x']]);
  "m_olp" @? (f two three [['a';'b';'c']; ['d';'e';'f']]
              = [%mat.char ['a','b','c'; 'd','e','f']]);
  "m_oll" @? (f two three [['a';'b';'c']; ['d';'e';'f']]
              = [%mat.char [['a';'b';'c']; ['d';'e';'f']]])

let test_mat_sint8 () =
  let f m n ll = of_list_dyn int8_signed m n ll in
  "m_emp" @? (f zero zero [] = [%mat.sint8 []]);
  "m_sgl" @? (f one  one  [[42]] = [%mat.sint8 [42]]);
  "m_olp" @? (f two three [[11;12;13]; [21;22;23]]
              = [%mat.sint8 [11,12,13; 21,22,23]]);
  "m_oll" @? (f two three [[11;12;13]; [21;22;23]]
              = [%mat.sint8 [[11;12;13]; [21;22;23]]])

let test_mat_uint8 () =
  let f m n ll = of_list_dyn int8_unsigned m n ll in
  "m_emp" @? (f zero zero [] = [%mat.uint8 []]);
  "m_sgl" @? (f one  one  [[42]] = [%mat.uint8 [42]]);
  "m_olp" @? (f two three [[11;12;13]; [21;22;23]]
              = [%mat.uint8 [11,12,13; 21,22,23]]);
  "m_oll" @? (f two three [[11;12;13]; [21;22;23]]
              = [%mat.uint8 [[11;12;13]; [21;22;23]]])

let test_mat_sint16 () =
  let f m n ll = of_list_dyn int16_signed m n ll in
  "m_emp" @? (f zero zero [] = [%mat.sint16 []]);
  "m_sgl" @? (f one  one  [[42]] = [%mat.sint16 [42]]);
  "m_olp" @? (f two three [[11;12;13]; [21;22;23]]
              = [%mat.sint16 [11,12,13; 21,22,23]]);
  "m_oll" @? (f two three [[11;12;13]; [21;22;23]]
              = [%mat.sint16 [[11;12;13]; [21;22;23]]])

let test_mat_uint16 () =
  let f m n ll = of_list_dyn int16_unsigned m n ll in
  "m_emp" @? (f zero zero [] = [%mat.uint16 []]);
  "m_sgl" @? (f one  one  [[42]] = [%mat.uint16 [42]]);
  "m_olp" @? (f two three [[11;12;13]; [21;22;23]]
              = [%mat.uint16 [11,12,13; 21,22,23]]);
  "m_oll" @? (f two three [[11;12;13]; [21;22;23]]
              = [%mat.uint16 [[11;12;13]; [21;22;23]]])

let test_mat_int () =
  let f m n ll = of_list_dyn int m n ll in
  "m_emp" @? (f zero zero [] = [%mat.int []]);
  "m_sgl" @? (f one  one  [[42]] = [%mat.int [42]]);
  "m_olp" @? (f two three [[11;12;13]; [21;22;23]]
              = [%mat.int [11,12,13; 21,22,23]]);
  "m_oll" @? (f two three [[11;12;13]; [21;22;23]]
              = [%mat.int [[11;12;13]; [21;22;23]]])

let test_mat_int32 () =
  let d = Int32.of_int in
  let f m n ll = of_list_dyn int32 m n ll in
  "m_emp" @? (f zero zero [] = [%mat.int32 []]);
  "m_sgl" @? (f one  one  [[d 42]] = [%mat.int32 [d 42]]);
  "m_olp" @? (f two three [[d 11;d 12;d 13]; [d 21;d 22;d 23]]
              = [%mat.int32 [d 11,d 12,d 13; d 21,d 22,d 23]]);
  "m_oll" @? (f two three [[d 11;d 12;d 13]; [d 21;d 22;d 23]]
              = [%mat.int32 [[d 11;d 12;d 13]; [d 21;d 22;d 23]]])

let test_mat_int64 () =
  let d = Int64.of_int in
  let f m n ll = of_list_dyn int64 m n ll in
  "m_emp" @? (f zero zero [] = [%mat.int64 []]);
  "m_sgl" @? (f one  one  [[d 42]] = [%mat.int64 [d 42]]);
  "m_olp" @? (f two three [[d 11;d 12;d 13]; [d 21;d 22;d 23]]
              = [%mat.int64 [d 11,d 12,d 13; d 21,d 22,d 23]]);
  "m_oll" @? (f two three [[d 11;d 12;d 13]; [d 21;d 22;d 23]]
              = [%mat.int64 [[d 11;d 12;d 13]; [d 21;d 22;d 23]]])

let suite =
  "%mat" >::: [
    "float32" >:: test_mat_float32;
    "float64" >:: test_mat_float64;
    "complex32" >:: test_mat_complex32;
    "complex64" >:: test_mat_complex64;
    "char" >:: test_mat_char;
    "sint8" >:: test_mat_sint8;
    "uint8" >:: test_mat_uint8;
    "sint16" >:: test_mat_sint16;
    "uint16" >:: test_mat_uint16;
    "int" >:: test_mat_int;
    "int32" >:: test_mat_int32;
    "int64" >:: test_mat_int64;
  ]
