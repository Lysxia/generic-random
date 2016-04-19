Generic random generators
=========================

Create a sized random generator for almost any type.

This will only work on types `a` such that:

- they are instances of `Data`;
- the set of types of subterms of values of type `a` is finite;
- and all of these types have at least one finite value (i.e., values with
  finitely many constructors) (otherwise our concept of "size" would not be
  meaningful).

The size of a value is its number of constructors.

The desired average `size` must be reachable, i.e., be larger than the smallest
value of type `a`, and smaller than the largest value of type `a` (when there
is one).

Examples of problematic types
-----------------------------

    data E a = L a | R (E [a])

Indeed, in `E`, every type of the form `[[...[[a]]...]]` occurs after
sufficiently many unwrappings of `R`'s.

    data I = C I

If we ignore bottoms, the only value of type `I` is an infinite stack of `C`
constructors.

References
----------

- The core theory of Boltzmann samplers is described in
  [Boltzmann Samplers for the Random Generation of Combinatorial Structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf),
  P. Duchon, P. Flajolet, G. Louchard, G. Schaeffer.

- The evaluation of generating functions defined by systems of equations is
  taken from
  [Boltzmann Oracle for Combinatorial Systems](http://www.dmtcs.org/pdfpapers/dmAI0132.pdf),
  C. Pivoteau, B. Salvy, M. Soria.
