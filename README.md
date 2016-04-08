Generic random generators
=========================

Create a sized random generator for almost any type.

This will only work on types `a` such that:

- they are instances of `Data`;
- they have a finite set of types which can be types of subterms of values
  of type `a`;
- and all of these types have finite values (i.e., values with finitely many
  constructors).

About the second point, phantom types are not a problem indeed.

An example of problematic type is:

    data E a = L a | R (E [a])

Indeed, every type of the form `[[...[[a]]...]]` occurs after sufficiently
many unwrappings of `R`'s.

The size of a value is its number of constructors.

The desired average `size` must be reachable, i.e., be larger than (or equal
to) the smallest value of type `a`, and smaller than (or equal to) the
largest value of type `a` (when there is one).

Reference
---------

- [Boltzmann Samplers for the Random Generation of Combinatorial Structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf).
