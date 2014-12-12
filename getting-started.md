---
layout: default
title: SLAP - Getting Started
---

Getting Started
===============

### How to install SLAP

You can install SLAP by using [OPAM](http://opam.ocamlpro.com/), which is a
package manager of OCaml;

```
$ opam install slap
```

or by hand:

```
$ git clone https://github.com/akabe/slap
$ cd slap
$ ./configure
$ make
$ sudo make install
```

In the latter case, you need to install dependent packages
[Lacaml](https://github.com/mmottl/lacaml) and
[cppo](http://mjambon.com/cppo.html) before installation of SLAP.

### Compiling your program

Here is a very simple program to get you started.
We assume that the name of this file is `example01.ml`.

```ocaml
(* File: example01.ml *)
open Slap.Size
open Slap.Io
open Slap.D

let () =
  let x = Vec.init three (fun i -> float_of_int i) in (* Create a three-dimensional vector. *)
  Format.printf "x = ( %a)@." pp_rfvec x (* Print the vector *)
```

We will explain the program after telling you how to compile it.

`ocamlfind` is useful to link SLAP and required packages.
You can byte-compile this program with a command like this:

```
$ ocamlfind ocamlc -linkpkg -package slap example01.ml
```

Similarily, you can native-compile it as follows:

```
$ ocamlfind ocamlopt -linkpkg -package slap example01.ml
```

When you run the compiled program, it shows the following output:

```
$ ./a.out
x = ( 1 2 3 )
```

### Using SLAP on toplevel

You can interactively use SLAP on the toplevel system of OCaml, a.k.a.,
REPL (Read-Eval-and-Print Loop). The default REPL of OCaml is inconvenient,
so that we recommend you to use UTop or OCaml REPL on Emacs.
Note that you require to load "topfind" at first:

```ocaml
# #use "topfind";;
# #require "slap.top";;
# open Slap.Size;;
# open Slap.Io;;
# open Slap.D;;
```

Let's try `example01.ml` on the toplevel environment.
First, create the vector `x`:

```ocaml
# let x = Vec.init three (fun i -> float_of_int i);;
val x : (z s s s, 'a) vec = R1 R2 R3
                             1  2  3
```

Next, print the vector:

```ocaml
# Format.printf "x = ( %a)@." pp_rfvec x;;
x = ( 1 2 3 )
- : unit = ()
```

Hereafter we show example programs and response from REPL for convenience.
You can byte- or native-compile the examples as well.

### Explanation of the first program

SLAP provides useful linear algebraic operations including BLAS and LAPACK
functions in the following modules.

- `Slap.S`: Single-precision (32-bit) real numbers
- `Slap.D`: Double-precision (64-bit) real numbers
- `Slap.C`: Single-precision (32-bit) complex numbers
- `Slap.Z`: Double-precision (64-bit) complex numbers

In `example01.ml`, we open `Slap.D`, thus, elements in vectors (and matrices)
are 64-bit real numbers. You can also use 32-bit or complex numbers by opening
other corresponding modules.

`Vec.init n f` creates a vector of the given dimension `n` and initializes
the i-th element in the vector by calling `f i`.
So `Vec.init three (fun i -> float_of_int i)` is the same as
`(float_of_int 1, float_of_int 2, float_of_int 3)`, i.e., `(1.0, 2.0, 3.0)`
where `(...)` is notation of a vector, not a tuple.
`three` is a special value for dimensions of vectors and matrices.
Sizes are provided by module `Slap.Size`.

Last, `Slap.Io` is a module providing pretty printers for vectors and matrices,
e.g., `Slap.Io.pp_rfvec` is a pretty printer for real row vectors. When you
print a vector (or a matrix), you use `%a` in a format string and pass a pretty
printer and a vector:

```ocaml
Format.printf "%a" pretty_printer vector
```

In `example01.ml`, `pretty_printer` = `pp_rfvec` and `vector` = `x`.
Other pretty printers are described in
http://akabe.github.com/slap/api/Slap.Io.html .

### Demonstration of static size checking

Have you tried the [demonstration of static size checking](index.html#demo) on
the home? If not so, we suggest you to try it because the most important feature
of SLAP is **static size checking** for vector and matrix operations.
SLAP detects dimensional inconsistency at compile time and helps you debug.
