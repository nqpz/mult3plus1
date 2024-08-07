# mult3plus1

I sometimes try to prove [the Collatz
conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture) by
programming my way out of it, but have not succeeded yet.


## symbolic

This program calculates the tree of what infinite subparts need to be
proved in order to prove that an integer $N > 1$ will always get smaller
when run through the calculation of the conjecture.

Try running `stack exec mult3plus1-symbolic` with one of these arguments:

  - `graph N`: Print the tree as a GraphViz dot graph.

  - `successrate N`: Print the percentage of successes.

  - `averagebranchingfactor N`: Print the average branching factor.

  - `depthfirst`: Do a depth-first search and print how many steps it
    takes to prove each subpart.

  - `depthfirstdelta`: Same as `depthfirst`, but print the deltas
    between the integers.  All deltas are probably positive.

  - `depthfirstdelta fibratio N`: The ratio of numbers in the delta that
    are Fibonacci numbers.  Seems pretty high (seems to reach 2 for
    larger values).

  - `depthfirstdelta fibratio comparerandom SEED`: Run `fibratio` on
    both the Collatz proof tree and on a random tree to compare the
    two ratios.

Where $N$ is a positive integer denoting search depth.


## modulo

Try running `stack exec mult3plus1-modulo N`, where N is a positive
integer.  This program will try to find the fraction of all integers
greater than 1 modulo $2^{N+1}$ that are guaranteed to become smaller
when run through the calculation of the conjecture.  The idea is that if
all these integers can be shown to have this property (unlikely), then
they will all end up at the integer 1 at the end, and the conjecture is
proven.
