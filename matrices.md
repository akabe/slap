---
layout: default
title: Matrices - SLAP
---

Matrices
========

Creating matrices
-----------------

A matrix can be created as follows.

{% highlight ocaml %}
(* a list of lists *)
# let a = [%mat [[1.0; 2.0; 3.0];
                 [4.0; 5.0; 6.0];
                 [7.0; 8.0; 9.0]]];;
val a : (three, three, 'a) mat =
     C1 C2 C3
  R1  1  2  3
  R2  4  5  6
  R3  7  8  9
(* a list of tuples *)
# let b = [%mat [1.0, 2.0, 3.0;
                 4.0, 5.0, 6.0;
                 7.0, 8.0, 9.0]];;
val b : (three, three, 'a) mat =
     C1 C2 C3
  R1  1  2  3
  R2  4  5  6
  R3  7  8  9
{% endhighlight %}

You can use functions to create a matrix as well.

{% highlight ocaml %}
# let a = Mat.make three five 42.0;;
val a : (three, five, 'a) mat =
     C1 C2 C3 C4 C5
  R1 42 42 42 42 42
  R2 42 42 42 42 42
  R3 42 42 42 42 42
{% endhighlight %}

`Mat.make` returns a matrix initialized by a given value.
Note that it takes two dimensions: the first parameter `three` is the number of
columns (i.e., the height) of the matrix and the second `five` is the number of
rows (i.e., the width). Thus `Mat.make three five x` returns a three-by-five
matrix initialized by the value of `x`.

Creating of matrices is the same as that of vectors except for passing two
dimensions. You can also use `Mat.create` (which returns an uninitialized matrix
for working memory), `Mat.make0` (which creates a matrix initialized by zero;
i.e., `Mat.make0 m n` is `Mat.make m n 0.0`), `Mat.make1` (which returns a
matrix initialized by one), `Mat.init` (which creates a matrix initialized by
a given function) and `Mat.random` (which returns a randomly-initialized
matrix). We show the following examples of `Mat.init` and `Mat.random`.

{% highlight ocaml %}
(* Mat.init *)
# let b = Mat.init four four (fun i j -> float_of_int (10 * i + j));;
val b : (z s s s s, z s s s s, 'a) mat =
     C1 C2 C3 C4
  R1 11 12 13 14
  R2 21 22 23 24
  R3 31 32 33 34
  R4 41 42 43 44
{% endhighlight %}

`Mat.init m n f` returns an `m`-by-`n` matrix whose element in the `(i,j)`
position is initialized by the result of calling of `f i j` (where `1 <= i <= m`
and `1 <= j <= n`).

{% highlight ocaml %}
(* Mat.random *)
# let b = Mat.random four four ~from:0.0 ~range:10.0;;
val b : (z s s s s, z s s s s, 'a) mat =
          C1      C2        C3      C4
  R1 9.26039 3.41639   5.97823 8.66882
  R2 7.86668 6.25315   0.90989  3.1101
  R3 5.59742 1.53848 0.0976622 7.56638
  R4 2.48343 6.52787   9.87177 1.67445
{% endhighlight %}

`Mat.random` is similar to `Vec.random`. The elements of a created matrix are in
interval `[from, from + range]`. Arguments `from` and `range` are optional.
If they are omitted, `-1.0` and `2.0` are passed, respectively, i.e., the
interval is `[-1.0, +1.0]`.

Matrix types
------------

Basically, the strange types of the matrices are the same as those of vectors.
Consider the type of the following matrix `a`, again.

{% highlight ocaml %}
# let a = Mat.make three five 42.0;;
val a : (three, five, 'a) mat =
     C1 C2 C3 C4 C5
  R1 42 42 42 42 42
  R2 42 42 42 42 42
  R3 42 42 42 42 42
{% endhighlight %}

- The first type parameter of the matrix type represents the number of columns
  (i.e., the height) of a matrix, and the second corresponds to the number
  of rows (i.e., the width). Types `three` (= `Slap.Size.three`), `five`
  (= `Slap.Size.five`) correspond to natural numbers "3" and "5", respectively.
  The size information of these type parameters is used for static size checking.
- The third type parameter is a "contiguous or discrete" flag. For the time
  being, you do not need to consider it because we only use contiguous
  matrices).

Matrix-vector operations
------------------------

Matrix-vector operations (Level 2 BLAS routines) are defined under `Slap.D`.
For example, `gemv` multiplies a general rectangular matrix and a vector.
`gemv ?beta ?y ~trans ?alpha a x` basically computes
`y := alpha * a * x + beta * y` where `alpha` (= 1 by default) and `beta`
(= 0 by default) are scalar values, `x` and `y` are vectors, and `a` is a
matrix:

{% highlight ocaml %}
# let x = [%vec [1.0; 2.0; 3.0; 4.0; 5.0]];;
val x : (five, 'a) vec = R1 R2 R3 R4 R5
                          1  2  3  4  5
# let a = [%mat [1.0, 2.0, 3.0, 4.0, 5.0;
                 2.0, 4.0, 6.0, 8.0, 10.0;
                 3.0, 6.0, 9.0, 12.0, 15.0]];;
val a : (three, five, 'a) mat =
     C1 C2 C3 C4 C5
  R1  1  2  3  4  5
  R2  2  4  6  8 10
  R3  3  6  9 12 15
# open Slap.Common;; (* Don't forget! *)
# gemv ~trans:normal a x;;
- : (three, 'a) vec = R1  R2  R3
                      55 110 165
{% endhighlight %}

The above example calculates

$$
  \\bm{A} \\bm{x} =
  \\begin{pmatrix}
    1 & 2 & 3 & 4 & 5 \\\\
    2 & 4 & 6 & 8 & 10 \\\\
    3 & 6 & 9 & 12 & 15
  \\end{pmatrix}
  \\begin{pmatrix}
    1 \\\\ 2 \\\\ 3 \\\\ 4 \\\\ 5
  \\end{pmatrix} =
  \\begin{pmatrix}
    55 \\\\ 110 \\\\ 165
  \\end{pmatrix}.
$$

The parameter `trans` indicates whether matrix `a` is transposed, or not:
when `normal` (= `Slap.Common.normal`) is passed, `a` is not transposed;
in contrast, `trans` (= `Slap.Common.trans`) means that `a` is transposed as
follows.

{% highlight ocaml %}
# let x' = [%vec [1.0; 2.0; 3.0]];;
val x' : (three, 'a) vec = R1 R2 R3
                            1  2  3
# gemv ~trans:trans a x';;
- : (five, 'a) vec = R1 R2 R3 R4 R5
                     14 28 42 56 70
{% endhighlight %}

The latter example computes

$$
  \\bm{A}^\top \\bm{x}' =
  \\begin{pmatrix}
    1 & 2 & 3 & 4 & 5 \\\\
    2 & 4 & 6 & 8 & 10 \\\\
    3 & 6 & 9 & 12 & 15
  \\end{pmatrix}^\top
  \\begin{pmatrix}
    1 \\\\ 2 \\\\ 3
  \\end{pmatrix} =
  \\begin{pmatrix}
    14 \\\\ 28 \\\\ 42 \\\\ 56 \\\\ 70
  \\end{pmatrix}.
$$

For example, you can use `gemv` for affine transformation.

Matrix-matrix operations
------------------------

Matrix-matrix operations (Level 3 BLAS routines) are defined under `Slap.D`
as well as matrix-vector operations. We only explain `gemm` and `symm` as
examples, but more routines are supported.

### gemm: multiplication of two general matrices

`gemm` multiplies two general rectangular matrices. Basically,
`gemm ?beta ?c ~transa ?alpha a ~transb b` executes
`c := alpha * a * b + beta * c` where `alpha` (= 1 by default) and `beta` (= 0
by default) are scalar values, and `a`, `b` and `c` are matrices:

{% highlight ocaml %}
# let a = [%mat [1.0, 2.0, 3.0, 4.0;
                 2.0, 4.0, 6.0, 8.0]];;
val a : (two, four, 'a) mat =     C1 C2 C3 C4
                               R1  1  2  3  4
                               R2  2  4  6  8
# let b = [%mat [2.0, 3.0, 4.0;
                 3.0, 4.0, 5.0;
                 4.0, 5.0, 6.0;
                 5.0, 6.0, 7.0]];;
val b : (four, three, 'a) mat =
     C1 C2 C3
  R1  2  3  4
  R2  3  4  5
  R3  4  5  6
  R4  5  6  7
# gemm ~transa:normal ~transb:normal a b;; (* a * b *)
- : (two, three, 'a) mat =    C1  C2  C3
                           R1 40  50  60
                           R2 80 100 120
{% endhighlight %}

The above code computes

$$
  \\bm{A} \\bm{B} =
  \\begin{pmatrix}
    1 & 2 & 3 & 4 \\\\
    2 & 4 & 6 & 8
  \\end{pmatrix}
  \\begin{pmatrix}
    2 & 3 & 4 \\\\
    3 & 4 & 5 \\\\
    4 & 5 & 6 \\\\
    5 & 6 & 7
  \\end{pmatrix} =
  \\begin{pmatrix}
    40 & 50 & 60 \\\\
    80 & 100 & 120
  \\end{pmatrix}.
$$

The parameters `transa` and `transb` indicate whether matrices `a` and `b` are
transposed, or not, respectively. They have the same effect as argument `trans`
of `gemv`. Differently from `gemv`, `gemm` takes two transpose flags because it
takes two matrices. As mentioned above, `a` is not transposed when `normal`
(= `Slap.Common.normal`) is given to `transa`. `trans` (= `Slap.Common.trans`)
means transposition as follows.

{% highlight ocaml %}
# gemm ~transa:trans ~transb:normal a a;; (* a^T * a *)
- : (four, four, 'a) mat =
   C1 C2 C3 C4
R1  5 10 15 20
R2 10 20 30 40
R3 15 30 45 60
R4 20 40 60 80
{% endhighlight %}

In this case, a four-by-four matrix is returned because
$\\bm{A}^\\top \\bm{A}$ is executed. Similarly, you can specify
whether `b` is transposed, or not though `transb`:

{% highlight ocaml %}
# gemm ~transa:normal ~transb:trans a a;; (* a * a^T *)
- : (two, two, 'a) mat =    C1  C2
                         R1 30  60
                         R2 60 120
{% endhighlight %}

The above example calculates $\\bm{A} \\bm{A}^\\top$.
Nothing to say, both `a` and `b` can be transposed at the same time.

### symm: multiplication of a symmetric matrix and a general matrix

`symm` multiplies a symmetric matrix and a general matrix. Basically,
`symm ~side ?up ?beta ?c ?alpha a b` executes `c := alpha * a * b + beta * c`
where `alpha` (= 1 by default) and `beta` (= 0 by default) are scalar values,
`a` is a symmetrix matrix, and `b` and `c` are general rectangular matrices.
`symm` does not have arguments to indicate transposition (e.g., `transa` and
`transb` of `gemm`) because a symmetric matrix is equal to the transpose of
itself. Instead of them, `symm` takes _side flags_ to specify direction of
the multiplication via argument `side`: when `side` = `Slap.Common.left`,
`a` is multiplied from the left by `b`, i.e., `c := alpha * a * b + beta * c`;
conversely, if `side` = `Slap.Common.right`, `a` is right-multiplied by `b`,
`c := alpha * b * a + beta * c`.

For example, the following code left-multiplies symmetric matrix `a` by
general matrix `b`.

{% highlight ocaml %}
# let a = [%mat [1.0, 2.0, 3.0;
                 2.0, 4.0, 6.0;
                 3.0, 6.0, 9.0]];;
val a : (three, three, 'a) mat =
     C1 C2 C3
  R1  1  2  3
  R2  2  4  6
  R3  3  6  9
# let b = [%mat [11.0, 12.0;
                 21.0, 22.0;
                 31.0, 32.0]];;
val b : (three, two, 'a) mat =    C1 C2
                               R1 11 12
                               R2 21 22
                               R3 31 32
# symm ~side:left a b;;
- : (z s s s, z s s, 'a) mat =     C1  C2
                               R1 146 152
                               R2 292 304
                               R3 438 456
{% endhighlight %}

It computes

$$
  \\bm{A} \\bm{B} =
  \\begin{pmatrix}
    1 & 2 & 3 \\\\
    2 & 4 & 6 \\\\
    3 & 6 & 9
  \\end{pmatrix}
  \\begin{pmatrix}
    11 & 12 \\\\
    21 & 22 \\\\
    31 & 32
  \\end{pmatrix} =
  \\begin{pmatrix}
    146 & 152 \\\\
    292 & 304 \\\\
    438 & 456
  \\end{pmatrix}.
$$

You can also right-multiply `a` by `b'`:

{% highlight ocaml %}
# let b' = [%mat [11.0, 12.0, 13.0;
                  21.0, 22.0, 23.0]];;
val b' : (two, three, 'a) mat =    C1 C2 C3
                                R1 11 12 13
                                R2 21 22 23
# symm ~side:right a b';;
- : (two, three, 'a) mat =     C1  C2  C3
                           R1  74 148 222
                           R2 134 268 402
{% endhighlight %}

The latter example computes

$$
  \\bm{B}' \\bm{A} =
  \\begin{pmatrix}
    11 & 12 & 13 \\\\
    21 & 22 & 23
  \\end{pmatrix}
  \\begin{pmatrix}
    1 & 2 & 3 \\\\
    2 & 4 & 6 \\\\
    3 & 6 & 9
  \\end{pmatrix} =
  \\begin{pmatrix}
     74 & 148 & 222 \\\\
    134 & 268 & 402
  \\end{pmatrix}.
$$

Optional argument `up` of `symm` specifies whether the upper or lower part of
`a` is used for computation. By default, `up` is `true`, so the upper part is
used, i.e., accessed by `symm`. If `up` is `false`, the lower part is supplied
for `symm`.

Iteration
---------

SLAP supports many iterator functions such as `map` and `fold_left` on matrices.
They are defined in `Slap.D.Mat`.
For example, `map` applies a given function to each element of a given matrix:

{% highlight ocaml %}
# let a = [%mat [11.0, 12.0, 13.0;
                 21.0, 22.0, 23.0]];;
val a : (two, three, 'a) mat =    C1 C2 C3
                               R1 11 12 13
                               R2 21 22 23
# Mat.map (fun aij -> 2.0 *. aij) a;;
- : (two, three, 'a) mat =    C1 C2 C3
                           R1 22 24 26
                           R2 42 44 46
{% endhighlight %}

(`Mat.scal` is faster than the above to multiply each element by a scalar
value.)

`fold_left` folds column vectors of a given matrix from left to right:

{% highlight ocaml %}
# Mat.fold_left (fun acc v -> v :: acc) [] a;;
- : (z s s, 'a) vec list = [R1 R2
                            13 23 ; R1 R2
                                    12 22 ; R1 R2
                                            11 21 ]
{% endhighlight %}

The above example creates a list containing column vectors of `a` in reverse.
(SLAP does not distinguish a column vector from a row vector on data structure.)
`fold_right`, `fold_top` (folding row vectors from top to bottom) and
`fold_bottom` (folding row vectors from bottom to top) are also supported.

Conversion into lists or arrays
-------------------------------

You can easily convert a matrix into a list or an array by `Mat.to_list` or
`Mat.to_array`.

{% highlight ocaml %}
# Mat.to_list a;;
- : float list list = [[11.; 12.; 13.]; [21.; 22.; 23.]]
# Mat.to_array a;;
- : float array array = [|[|11.; 12.; 13.|]; [|21.; 22.; 23.|]|]
{% endhighlight %}

However, conversion into a matrix from a list or an array is not as easy as
them. We will describe such conversion in a later chapter.

Index-based access
------------------

You can access an element in a matrix with indices by `Mat.get_dyn` and
`Mat.set_dyn`.

{% highlight ocaml %}
# Mat.get_dyn a 1 2;; (* Get the (1,2) element of `a' *)
- : float = 12.
# Mat.set_dyn a 1 2 100.;; (* Modify the (1,2) element of `a' *)
- : unit = ()
# a;;
- : (z s s, z s s s, 'a) mat =    C1  C2 C3
                               R1 11 100 13
                               R2 21  22 23
{% endhighlight %}

SLAP cannot statically verify whether given indices is valid, or not.
`get_dyn` and `set_dyn` functions are _low-level operations_ in SLAP. You should
use high-level functions like `map` or arithmetic operations instead of them
whenever possible because maybe static size checking does not work well. We will
explain the reason in a later chapter.
(In addition, `get_dyn` and `set_dyn` are not fast.)
