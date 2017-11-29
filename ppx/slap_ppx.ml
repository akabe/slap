(* Sized Linear Algebra Package (SLAP)

   Copyright (C) 2013- Akinori ABE <aabe.65535@gmail.com>

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

(** PPX syntactic extensions for SLAP. (This is a dummy module.)

    For use of the extensions, you need to pass [-ppx ppx_slap] to the OCaml
    compiler, or specify [-package slap.ppx] to OCamlfind. You can also use by

{[#use "topfind";;
#require "slap.ppx";;]}

    on interactive mode.

    {2 Literals}

    {3 Vector literals}

    Vector literals like [[%vec.kind [e1; e2; ...; eN]]] are supported where
    [e1] ... [eN] are elements and [kind] specifies the type and precision
    of the elements. It corresponds to [Bigarray] kind:

    - [float32], [S] or [s]: 32-bit real number ([Bigarray.float32])
    - [float64], [D] or [d]: 64-bit real number ([Bigarray.float64])
    - [complex32], [C] or [c]: 32-bit complex number ([Bigarray.complex32])
    - [complex64], [Z] or [z]: 64-bit complex number ([Bigarray.complex64])
    - [int8_signed] or [sint8]: 8-bit signed integer ([Bigarray.int8_signed])
    - [int8_unsigned] or [uint8]: 8-bit unsigned integer
      ([Bigarray.int8_unsigned])
    - [int16_signed] or [sint16]: 16-bit signed integer
      ([Bigarray.int16_signed])
    - [int16_unsigned] or [uint16]: 16-bit unsigned integer
      ([Bigarray.int16_unsigned])
    - [int]: 31-bit integer ([Bigarray.int])
    - [int32]: 32-bit integer ([Bigarray.int32])
    - [int64]: 64-bit integer ([Bigarray.int64])
    - [nativeint]: native integer ([Bigarray.nativeint])
    - [char]: character ([Bigarray.char])

    You can omit a kind if you open [Slap.[SDCZ]]:

    - [[%vec [...]]] (referring [Slap.[SDCZ].prec] as [Bigarray] kind,
      corresponding to an opened module)
    - [[%rvec [...]]] (referring [Slap.[SDCZ].rprec] as [Bigarray] kind,
      corresponding to an opened module)

    For examples, [[%vec.float64 [42.0; 123.0; 456.0]]] is a three-dimensional
    vector that has 64-bit real numbers [42.0], [123.0] and [456.0]. If you open
    [Slap.D], the vector literal is the same as [[%vec [42.0; 123.0; 456.0]]].

    {3 Matrix literals}

    Matrix literals can be written as

{[[%mat.kind [ [e11; e12; ...; e1N];
             [e21; e22; ...; e2N];
             [...; ...; ...; ...];
             [eM1; eM2; ...; eMN] ]]]}

    or

{[[%mat.kind [ e11, e12, ..., e1N;
             e21, e22, ..., e2N;
             ..., ..., ..., ...;
             eM1, eM2, ..., eMN ]]]}

    where [kind] is one of the above-mentioned identifiers. You can also use
    [[%mat ...]] and [[%rmat ...]] (referring [Slap.[SDCZ].prec] and
    [Slap.[SDCZ].rprec] respectively) as similar to vector literals. *)
