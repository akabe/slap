Sized Linear Algebra Package (SLAP)
===================================

This [OCaml](http://ocaml.org/)-library is a wrapper of
[Lacaml](https://bitbucket.org/mmottl/lacaml), a binding of two widely used
linear algebra libraries [BLAS](http://www.netlib.org/blas/) (Basic Linear
Algebra Subprograms) and [LAPACK](http://www.netlib.org/lapack/) (Linear Algebra
PACKage) for FORTRAN.
SLAP guarantees *statically* (i.e, at compile time) consistency (with respect to
dimensions) of most high-level matrix (and vector) operations by using
*generative phantom types*. For example, addition of two- and three-dimensional
vectors causes type error at compile time, and dynamic errors like exceptions do
not happen. (Certain low-level operations, like accesses to elements by indices,
need dynamic checks.)

This provides many useful and high-performance linear algebra operations in
Lacaml, e.g., least squares problems, linear equations, Cholesky,
QR-factorization, eigenvalue problems and singular value decompisition (SVD).
Most of their interfaces are compatible with Lacaml functions.
(Several functions have not been implemented yet, but they will be supported
soon.)

An [online API documentation](http://akabe.github.io/slap/) is available.
