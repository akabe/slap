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
[Lacaml](https://github.com/mmottl/lacaml) (version 7.2.3 or above) and
[cppo](http://mjambon.com/cppo.html) before installation of SLAP.

### Compiling your program

Here is a very simple program to get you started.
We assume that the name of this file is `example01.ml`.

{% highlight ocaml %}
(* File: example01.ml *)
open Slap.Size
open Slap.Io
open Slap.D

let () =
  let x = [%vec [1.0; 2.0; 3.0]] in (* Create a three-dimensional vector. *)
  Format.printf "x = ( %a)@." pp_rfvec x (* Print the vector *)
{% endhighlight %}

We will explain the program after telling you how to compile it.

`ocamlfind` is useful to link SLAP and required packages.
You can byte-compile this program with a command like this:

```
$ ocamlfind ocamlc -package slap,slap.ppx -linkpkg -short-paths example01.ml
```

`-short-paths` option makes type error messages more readable.

Similarily, you can native-compile it as follows. We recommend to use
native-compilation because byte-compiled programs are much slower than
native-compiled ones.

```
$ ocamlfind ocamlopt -package slap,slap.ppx -linkpkg -short-paths example01.ml
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

{% highlight ocaml %}
# #use "topfind";;
# #require "slap.top";;
# #require "slap.ppx";;
# open Slap.Size;;
# open Slap.Io;;
# open Slap.D;;
{% endhighlight %}

Let's try `example01.ml` on the toplevel environment.
First, create the vector `x`:

{% highlight ocaml %}
# let x = [%vec [1.0; 2.0; 3.0]];;
val x : (three, 'a) vec = R1 R2 R3
                           1  2  3
{% endhighlight %}

Next, print the vector:

{% highlight ocaml %}
# Format.printf "x = ( %a)@." pp_rfvec x;;
x = ( 1 2 3 )
- : unit = ()
{% endhighlight %}

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

`[%vec [x1; x2; ...; xN]]` is the `N`-dimensional vector whose elements are
`x1`, `x2`, ..., and `xN`. Thus `[%vec [1.0; 2.0; 3.0]]` is three-dimensional
vector $(1.0, 2.0, 3.0)$.

`Slap.Io` is a module providing pretty printers for vectors and matrices,
e.g., `Slap.Io.pp_rfvec` is a pretty printer for real row vectors. When you
print a vector (or a matrix), you use `%a` in a format string and pass a pretty
printer and a vector:

{% highlight ocaml %}
Format.printf "%a" pretty_printer vector
{% endhighlight %}

In `example01.ml`, `pretty_printer` = `pp_rfvec` and `vector` = `x`.
Other pretty printers are described in
http://akabe.github.com/slap/api/Slap.Io.html .

### Demonstration of static size checking

Have you tried the [demonstration of static size checking](index.html#demo)?
If not so, we suggest you to try it because the most important feature
of SLAP is **static size checking** for vector and matrix operations.
SLAP detects dimensional inconsistency at compile time and helps your debug.
