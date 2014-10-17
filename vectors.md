---
layout: default
title: Vectors - SLAP
---

Vectors
=======

Vector types
------------

Consider the type of the following vector `x`.

```ocaml
# let x = Vec.init three (fun i -> float_of_int i);;
val x : (z s s s, 'a) vec = R1 R2 R3
                             1  2  3
```

The vector has a curious type:

- The first type parameter of the vector type represents the dimension of a
  vector. The types `z` and `'n s` correspond to zero and `'n + 1`,
  respectively. Thereby, `z s s s` represents 0+1+1+1 = 3, thus
  `(z s s s, 'a) vec` is the type of three-dimensional vectors. The size
  information of this type parameter is used for static size checking.
- The second type parameter is a "contiguous or discrete" flag. For the time
  being, you do not need to consider it because we only use contiguous
  vectors (and matrices).

`three` (= `Slap.Size.three`), which is passed to the first argument of
`Vec.init`, is **not** a normal integer: See the type of it.

```ocaml
# three;;
- : z s s s t = 3
```

The value is three, but its type is `'n t` (= `'n Slap.Size.t`), not `int`. This
is a special value to represent a _size_, i.e., a natural number as dimensions
of vectors and matrices. The above type `z s s s t` is the type of three and
terms (i.e., expressions) evaluated to three. In SLAP, you need to pass a
size to `Vec.init` as a dimension instead of an integer.

Creating vectors
----------------

We have already explained `Vec.init`, which creates a vector initialized by a
given function. SLAP supports other functions to create vectors. The simplest
function is `Vec.create`. It creates an uninitialized vector. You need to
initialize the elements of a returned vector by hand. It is effective when you
allocate working or temporary memory because there is no overhead for
initialization.

If you initialize all elements in a vector by the same value, `Vec.make` is
useful:

```ocaml
# Vec.make three 42.0;;
- : (z s s s, 'a) vec = R1 R2 R3
                        42 42 42
```

This is faster than `Vec.init` for large vectors. When the initial value is zero
or one, you can use `Vec.make0` or `Vec.make1` instead of `Vec.make`. They
correspond to `zeros` or `ones` in MatLab.

```ocaml
# Vec.make0 three;;
- : (z s s s, 'a) vec = R1 R2 R3
                         0  0  0
# Vec.make1 three;;
- : (z s s s, 'a) vec = R1 R2 R3
                         1  1  1
```

You can create a randomly-initialized vector using `Vec.random`.

```ocaml
# Vec.random ~from:(-10.) ~range:20. three;;
- : (z s s s, 'a) vec =      R1       R2      R3
                        8.52078 -3.16723 1.95646
```

The elements of a created vector are in interval `[from, from + range]`.
Arguments `from` and `range` are optional: If they are omitted, `-1.0` and `2.0`
are passed, respectively.

Vector-vector operations
------------------------

### Original functions

We introduce several major element-wise operations like arithmetic operations in
this section. Here is an example of `Vec.add` for element-wise addition.

```ocaml
# let x = Vec.init three (fun i -> float_of_int (10 * i));;
val x : (z s s s, 'a) vec = R1 R2 R3
                            10 20 30
# let y = Vec.make1 three;;
val y : (z s s s, 'a) vec = R1 R2 R3
                             1  1  1
# Vec.add x y;;
- : (z s s s, 'a) vec = R1 R2 R3
                        11 21 31
```

This is addition of two vectors in linear algebra. In addition, element-wise
subtraction `Vec.sub`, multiplication `Vec.mul`, division `Vec.div`, square root
`Vec.sqrt`, exponent `Vec.exp`, logarithm `Vec.log`, etc. are supported.

```ocaml
# Vec.div y x;;
- : (z s s s, 'a) vec =  R1   R2        R3
                        0.1 0.05 0.0333333
# Vec.log x;;
- : (z s s s, 'a) vec =      R1      R2     R3
                        2.30259 2.99573 3.4012
...
```

### Level 1 BLAS

In BLAS, vector-vector operations are defined as _Level 1_ under `Slap.D`, not
`Slap.D.Vec`. For example, `axpy alpha ~x y` computes `y := alpha * x + y` with
a scalar value `alpha`, and vectors `x` and `y`.

```ocaml
# axpy ~alpha:0.5 ~x y;;
- : unit = ()
# y;;
- : (z s s s, 'a) vec = R1 R2 R3
                         6 11 16
```

In this case, `x` is `(10, 20, 30)` and `y` is `(1, 1, 1)`.
`axpy ~alpha:0.5 ~x y` computes `0.5 * (10, 20, 30) + (1, 1, 1)` and
destructively assigns the result `(6, 11, 16)` to `y`.
In addition, `copy`, `swap` (to swap elements in two vectors), `scal`
(to multiply a scalar value to al elements in a vector), etc. are
provided.

Iteration
---------

SLAP supports many iterator functions such as `map` and `fold_left` on vectors.
They are like `List.map` and `List.fold_left` in the OCaml standard library, but
SLAP provides more operations, e.g., `map3`. Here we introduce only two examples
of `Vec.map` and `Vec.iter`.

`Vec.map` applies a given function to each element in a given vector.

```ocaml
# Vec.map (fun xi -> 2.0 *. xi) x;;
- : (z s s s, 'a) vec = R1 R2 R3
                        20 40 60
```

`Vec.iter` calls a given function for each element in a given vector repeatedly.

```ocaml
# Vec.iter (fun xi -> Format.printf "elem = %G@." xi) x;;
elem = 10
elem = 20
elem = 30
- : unit = ()
```

Conversion into lists or arrays
-------------------------------

You can easily convert a vector into a list or an array by `Vec.to_list` or
`Vec.to_array`.

```ocaml
# Vec.to_list x;;
- : float list = [10.; 20.; 30.]
# Vec.to_array x;;
- : float array = [|10.; 20.; 30.|]
```

However, conversion into a vector from a list or an array is not as easy as
them. We will describe such conversion in a later chapter.

Index-based access
------------------

You can access an element in a vector with indices by `Vec.get_dyn` and
`Vec.set_dyn`.

```ocaml
# Vec.get_dyn x 2;; (* Get the second element of `x` *)
- : float = 20.
# Vec.set_dyn x 2 100.;; (* Modify the second element of `x' *)
- : unit = ()
# x;;
- : (z s s s, 'a) vec = R1  R2 R3
                        10 100 30
```

SLAP cannot statically verify whether a given index is valid, or not, i.e.
`1 <= i <= n` where `n` is the dimension of a vector and `i` is the index.
So `Vec.get_dyn` and `Vec.set_dyn` raises an exception if the above condition
is not satisfied. (We gave functions containing dynamic checks the suffix
`_dyn`.)

```ocaml
Vec.get_dyn x 10;;
Exception: Invalid_argument "Slap.Vec.get_dyn".
```

`get_dyn` and `set_dyn` functions are _low-level operations_ in SLAP. You should
use high-level functions like `map` or arithmetic operations instead of them
whenever possible because maybe static size checking does not work well. We will
explain the reason in a later chapter.
(In addition, `get_dyn` and `set_dyn` are not fast.)

Detection of dimensional inconsistency
--------------------------------------

Let's create a three- and a four-dimensional vectors as follows.

```ocaml
# let x = Vec.init three (fun i -> float_of_int i);;
val x : (z s s s, 'a) vec = R1 R2 R3
                             1  2  3
# let z = Vec.init four (fun i -> float_of_int i);;
val z : (z s s s s, 'a) vec = R1 R2 R3 R4
                               1  2  3  4
```

The types of vectors `x` and `z` are different.
In SLAP, vectors of different dimensions **always** have different types.
Nothing to say, addition of `x` and `z` is undefined in general linear algebra.
Such inconsistency of dimensions causes a type error like this:

```ocaml
# Vec.add x z;;
Error: This expression has type
(z s s s s, 'a) vec = (z s s s s, float, rprec, 'a) Slap.Vec.t
but an expression was expected of type
(z s s s, 'b) vec = (z s s s, float, rprec, 'b) Slap.Vec.t
Type z s is not compatible with type z
```

We explain the meaning of this error message. `Vec.add` has the following type.

```ocaml
val Vec.add : ('n, _) vec -> ('n, _) vec -> ('n, _) vec
```

This represents the function gets two `'n`-dimensional vectors and returns a
`'n`-dimensional vector. However `x` has type `(z s s s, 'a) vec` and `z` has
type `(z s s s s, 'a) vec`. OCaml says that type parameter `'n` is instantiated
to `z s s s` and `z s s s s`, but they are incompatible (i.e., different).
Dimensional inconsistency is detected as a type error at compile time like that.
