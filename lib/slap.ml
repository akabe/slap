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

(** A linear algebra library with type-based static size checking for matrix
    operations. SLAP (Sized Linear Algebra Package) helps debug of your programs
    by detecting dimensional inconsistency, e.g., addition of two- and three-
    dimensional vectors, at {b compile time} (instead of runtime) and at {b
    higher level} (i.e., at a caller site rather than somewhere deep inside of a
    call stack). This library is a wrapper of
    {{:https://github.com/mmottl/lacaml}LACAML}, a binding of widely-used linear
    algebra libraries {{:http://www.netlib.org/blas/}BLAS (Basic Linear Algebra
    Subprograms)} and {{:http://www.netlib.org/lapack/}LAPACK (Linear Algebra
    PACKage)} in FORTRAN. Many functions are compatible with the interface of
    LACAML. You can make use of SLAP by referring {!Slap.Common} and the module
    corresponding to the precision and the number type you need:

{[open Slap.Common
open Slap.S  (* 32-bit real BLAS and LAPACK functions *)
open Slap.D  (* 64-bit real BLAS and LAPACK functions *)
open Slap.C  (* 32-bit complex BLAS and LAPACK functions *)
open Slap.Z  (* 64-bit complex BLAS and LAPACK functions *)]}

    To use this library, the following documentation may be useful to you:

    - {{:https://github.com/akabe/slap/tree/master/examples}Example programs}
    - {{:http://akabe.github.io/slap/usage.html}Tutorial of SLAP}
    - {{:http://mmottl.github.io/lacaml/api/}LACAML API documentation}
    - {{:http://www.netlib.org/blas/blasqr.ps}A quick reference guide for BLAS}
    - {{:http://www.netlib.org/lapack/lapackqref.ps}LAPACK quick reference guide
    to the driver routines}
    - {{:http://www.math.utah.edu/software/lapack/}The man pages of BLAS and
    LAPACK}

    The man pages are also available on your machine if
    you installed {{:http://www.netlib.org/lapack/manpages.tgz}manpages.tgz} or
    the corresponding package provided by your distribution. *)

(** Miscellaneous definitions. *)
include Slap_misc

(** Sizes (the dimensions of vectors and matrices). *)
module Size = Slap_size

(** Sized vectors. *)
module Vec = Slap_vec

(** Sized matrices. *)
module Mat = Slap_mat

(** Pretty printers. *)
module Io = Slap_io

(** The version number of SLAP.
    @since 2.0.0 *)
module Version = Slap_version

(** {2 Precision dependent modules} *)

(** Types, flags and functions commonly used in precision dependent modules. *)
module Common = Slap_common

(** 64-bit real BLAS and LAPACK functions. *)
module D = Slap_D

(** 32-bit real BLAS and LAPACK functions. *)
module S = Slap_S

(** 64-bit complex BLAS and LAPACK functions. *)
module Z = Slap_Z

(** 32-bit complex BLAS and LAPACK functions. *)
module C = Slap_C
