---
layout: default
title: Dimensions that are unknown until runtime - SLAP
---

Dimensions that are unknown until runtime
=========================================

We have introduced vectors and matrices whose dimensions were statically decided
so far. SLAP can statically verify dimensional consistency when the sizes are
**dynamically** determined, e.g., when a vector (or a matrix) is loaded from a
file at runtime.
Unfortunately, however, SLAP does not provide functions to load it from a file.
You need to write a function to load a list or an array from a file and convert
it into a vector or a matrix for the time being. Please let us know (or please
send us a pull request) if you want such functions.

Conversion of lists or arrays into vectors
------------------------------------------

`Vec.of_list` converts a list into a vector:

{% highlight ocaml %}
# module X = (val Vec.of_list [1.0; 2.0; 3.0] : Vec.CNTVEC);;
module X : Slap.D.Vec.CNTVEC
{% endhighlight %}

Oh, a module is returned. The module `X` has the following signature.

{% highlight ocaml %}
module type Vec.CNTVEC =
sig
  type n (* The type that represents the dimension of a vector *)
  val value : (n, _) vec (* The instance of a vector *)
end
{% endhighlight %}

The instance of a vector is stored as `X.value`:

{% highlight ocaml %}
# X.value;;
- : (X.n, 'cnt) vec = R1 R2 R3
                       1  2  3
{% endhighlight %}

The vector `X.value` can be used as the vectors that we have introduced so far.
For example,

{% highlight ocaml %}
# Vec.map log X.value;;
- : (X.n, 'a) vec = R1       R2      R3
                     0 0.693147 1.09861
{% endhighlight %}

Similarly, `Vec.of_array` converts an array into a vector.

{% highlight ocaml %}
# module Y = (val Vec.of_array [|1.0; 2.0; 3.0|] : Vec.CNTVEC);;
module Y : Slap.D.Vec.CNTVEC
{% endhighlight %}

You can pass lists whose sizes are dynamically decided as well. For example, the
following code reads an integer from standard input and creates the vector
whose dimension is equal to the integer. The dimension is obtained at runtime,
thus it cannot be decided at compile time.

{% highlight ocaml %}
# module Z = (val Vec.of_array (Array.make (read_int ()) 42.0) : Vec.CNTVEC);;
{% endhighlight %}

The module `Z` containing a vector of the dimension that is unknown until runtime
will be returned after your input. Let us say the input is `6`:

{% highlight ocaml %}
6
module Z : Slap.D.Vec.CNTVEC
{% endhighlight %}

`Z` contains a six-dimensional vector as follows.

{% highlight ocaml %}
# Z.value;;
- : (Z.n, 'cnt) vec = R1 R2 R3 R4 R5 R6
                      42 42 42 42 42 42
{% endhighlight %}

We will describe the mechanism of static size checking for the vectors (and
matrices) later.

Conversion of lists or arrays into matrices
-------------------------------------------

`Mat.of_list` converts a list of lists into a matrix:

{% highlight ocaml %}
# module A = (val Mat.of_list
                [[1.0; 2.0; 3.0];
                 [2.0; 3.0; 4.0]]
              : Mat.CNTMAT);;
module A : Slap.D.Mat.CNTMAT
{% endhighlight %}

The module `A` has the following signature.

{% highlight ocaml %}
module type Vec.CNTVEC =
sig
  type m (* The type that represents the number of rows in a matrix *)
  type n (* The type that represents the number of columns in a matrix *)
  val value : (m, n, _) mat (* The instance of a matrix *)
end
{% endhighlight %}

`A.value` is the following matrix.

{% highlight ocaml %}
# A.value;;
- : (A.m, A.n, 'cnt) mat =    C1 C2 C3
                           R1  1  2  3
                           R2  2  3  4
{% endhighlight %}

You can also use `Mat.of_array` to convert an array of arrays into a matrix.

Static size checking
--------------------

In this section, we explain the reason the above-mentioned way prevents
dimensional inconsistency. For example, consider function
`loadvec : string -> (?, _) vec` to load a vector of **some** dimension, loaded
from the given path. Nothing to say, the dimension of a returned vector is
determined at **runtime**, but we need to give it some type at **compile time**.
The main problem is how to represent `?` (which is unknown until runtime).
Consider the following code for example:

{% highlight ocaml %}
let (x : (?1, _) vec) = loadvec "file1" in
let (y : (?2, _) vec) = loadvec "file2" in
dot x y (* should be ill-typed! *)
{% endhighlight %}

`dot x y` at the third line should be ill-typed because the dimensions of `x`
and `y` are probably different, i.e., `?1` and `?2` should be different types.
How about the case that the two paths are the same?

{% highlight ocaml %}
let (x : (?1, _) vec) = loadvec "file" in
let (y : (?2, _) vec) = loadvec "file" in
dot x y (* should be ill-typed! *)
{% endhighlight %}

In this case, `dot x y` should be ill-typed because the file might change
between two loads, thus `?1` and `?2` need to be different as well. In summary,
the return type of `loadvec` should be **different** every time it is called
(regardless the specific values of the argument). We call such a return type
*generative* because the function returns a value of a fresh type for each
call. The vector type with generative size information essentially corresponds
to an existentially quantified sized type like `exists n. n vec`.

The type parameters `'m`, `'n` and `'a` of types `'n Size.t`, `('n, 'a) vec` and
`('m, 'n, 'a) mat` are *phantom*, meaning that they do not appear on the right
hand side of the type definition. A phantom type parameter is often instantiated
with a type that has no value (i.e., no constructor) which we call a
*phantom type*, thus we call the type `?` a *generative phantom type*.

Actually, `Vec.of_list`, `Vec.of_array`, `Mat.of_list` and `Mat.of_array` behave
like `loadvec`. They return a module containing a vector and the type that
representing dimension such as `X.n`, `A.m`, `A.n`, etc. The type in a returned
module becomes **different** every time the function is called. For example,
the following code makes two modules `X` and `Y` containing vectors.

{% highlight ocaml %}
# module X = (val Vec.of_list [1.0; 2.0; 3.0] : Vec.CNTVEC);;
module X : Slap.D.Vec.CNTVEC
# module Y = (val Vec.of_list [4.0; 5.0] : Vec.CNTVEC);;
module Y : Slap.D.Vec.CNTVEC
{% endhighlight %}

`X.value` has type `(X.n, _) vec` and `Y.value` has type `(Y.n, _) vec`.
The computation of the inner product of `X.value` and `Y.value` causes
type error as follows because `X.n` and `Y.n` are different types.

{% highlight ocaml %}
# dot X.value Y.value;;
Error: This expression has type
  (Y.n, 'a) vec = (Y.n, float, rprec, 'a) Slap.Vec.t
but an expression was expected of type
  (X.n, 'b) vec = (X.n, float, rprec, 'b) Slap.Vec.t
Type Y.n is not compatible with type X.n
{% endhighlight %}

Even if the dimensions of `X.value` and `Y.value` were the same by some chance,
`dot X.value Y.value` would be ill-typed in the above-mentioned reason.

You can compute addition, subtraction, inner product, etc. of the vectors and
matrices only if their dimensions are always equal. For example, the following
code computes the inner product of `u` and `X.value`.

{% highlight ocaml %}
# let u = Vec.map (fun xi -> 1.0 -. xi *. xi) X.value;;
val u : (X.n, 'a) vec = R1 R2 R3
                         0 -3 -8
# dot u X.value;;
- : float = -30.
{% endhighlight %}

`Vec.map` has type `(float -> float) -> ('n, _) vec -> ('n, _) vec`. It
indicates that `Vec.map` always returns a vector of the same dimension as an
argument, so that `u` has the same type as `X.value`.

Using dynamic checks together
-----------------------------

Maybe you want to add vectors loaded from different files. `Vec.of_list_dyn` is
supplied for the situation.

{% highlight ocaml %}
val Vec.of_list_dyn : 'n Size.t -> float list -> ('n, _) vec
{% endhighlight %}

It is also a function to convert a list into a vector, but different from
`Vec.of_list`. It takes a size value corresponding the length of a given list as
the first parameter. For example, you can convert lists `lst1` and `lst2` loaded
from different files into vectors and add them as follows:

{% highlight ocaml %}
# module X = (val Vec.of_list lst1 : Vec.CNTVEC);;
# let y = Vec.of_list_dyn (Vec.dim X.value) lst2;;
# Vec.add X.value y;;
{% endhighlight %}

`dim : ('n, _) vec -> 'n Size.t` returns the dimension of a given vector.
If `lst1` and `lst2` have different lengths, `Vec.of_list_dyn` raises an
**exception**. We cannot guarantee the equality of dimensions of vectors
loaded from different files at compile time, so that this dynamic check is
unavoidable. We gave the suffix `_dyn` to functions containing unavoidable
dynamic checks.

Returning vectors and matrices that have generative phantom types
-----------------------------------------------------------------

The vectors and matrices of the dimensions that are dynamically determined can
be created in local scope as follows:

{% highlight ocaml %}
# let sum_list lst =
    let module X = (val Vec.of_list lst : Vec.CNTVEC) in (* a local module *)
    Vec.sum X.value;;
val sum_list : float list -> float = <fun>
{% endhighlight %}

`sum_list` calculates the sum of all elements in a given list. The vector
`X.value` is used internally and does not escape its scope.

If such vector escapes its scope, contrivance is required. For example,
the following function `vec_of_string_array` converts an array of strings
into a vector.

{% highlight ocaml %}
# let vec_of_string_array a =
    let module N = (val of_int_dyn (Array.length a) : SIZE) in
    Vec.init N.value (fun i -> float_of_string a.(i-1));;
{% endhighlight %}

`Slap.Size.of_int_dyn` converts an integer to a size `N.value : N.n Size.t`.
The function directly returns vector `X.value : (N.n, _) vec`, thus the type of
it is likely `string array -> (N.n, _) vec` intuitively. However, the return
type `(N.n, _) vec` contains the locally defined identifier `N`. Accordingly,
the function cannot be typed, and OCaml outputs the following error message.

```
Error: This expression has type
         (N.n, 'a) vec = ('b, float, rprec, 'c) Slap.Vec.t
       but an expression was expected of type (N.n, 'a) vec
       The type constructor N.n would escape its scope
```

There are two ways to handle this in SLAP.

### 1. Using first-class modules

The first way is to use a first-class module to return a generative phantom
type:

{% highlight ocaml %}
# let vec_of_string_array a =
    let module N = (val of_int_dyn (Array.length a) : SIZE) in
    (module struct
       type n = N.n
       let value = Vec.init N.value (fun i -> float_of_string a.(i-1))
     end : Vec.CNTVEC);;
val vec_of_string_array : bytes array -> (module Slap.D.Vec.CNTVEC) = <fun>
{% endhighlight %}

The syntax is slightly heavy, but the idea is simple: a module containing a
generative phantom type is returned instead of a vector. The above function
can be used like `Vec.of_array`:

{% highlight ocaml %}
# let main () =
    let module X = (val vec_of_string_array [|"1.0"; "2.0"; "3.0"|] : Vec.CNTVEC) in
    printf "X.value = [ %a]@." pp_rfvec X.value;;
val main : unit -> unit = <fun>
# main ();;
X.value = [ 1 2 3 ]
- : unit = ()
{% endhighlight %}

### 2. Adding extra arguments

Another is to insert the argument `n` for the size of the array, and remove the
generative phantom type from the function:

{% highlight ocaml %}
# let vec_of_string_array n a =
    if to_int n <> Array.length a then invalid_arg "error";
    Vec.init n (fun i -> float_of_string a.(i-1));;
val vec_of_string_array : 'a Size.t -> bytes array -> ('a, 'b) vec = <fun>
{% endhighlight %}

In this case, programming is easier than the former way because the code is
simple, but whether `n` is equal to the length of a should be dynamically
checked.

### Trade-off of two solutions

Both solutions have merits and demerits. In practical cases, they are in a
trade-off relationship:

|                        | generative phantom types         | static size checking | programming     |
|:-----------------------|:--------------------------------:|:--------------------:|:---------------:|
| 1. extra arguments     | given from outside of a function | no                   | **easy**        |
| 2. first-class modules | created in a function            | **yes**              | (slightly) hard |
