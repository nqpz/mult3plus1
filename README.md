# mult3plus1

I sometimes try to prove [the Collatz
conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture) by
programming my way out of it, but have not succeeded yet.


## modulo

Try running `stack run mult3plus1-modulo N`, where N is a positive
integer.  This program will try to find the fraction of all integers
greater than 1 modulo $2^{N+1}$ that are guaranteed to become smaller
when run through the calculation of the conjecture.  The idea is that if
all these integers can be shown to have this property (unlikely), then
they will all end up at the integer 1 at the end, and the conjecture is
proven.


## symbolic

This program calculates the tree of what infinite subparts need to be
proved in order to prove that an integer $N > 1$ will always get smaller
when run through the calculation of the conjecture.

Try running `stack run mult3plus1-symbolic` with one of these arguments:

  - `print N`: Print the tree.

  - `shape N`: Print the shape of the tree.

  - `percent N`: Print the percentage of successes.

  - `depthfirst`: Do a depth-first search and print how many steps it
    takes to prove each subpart.

  - `depthfirstdelta`: Same as `depthfirst`, but print the deltas
    between the integers.  All deltas are probably positive.

Where N is a positive integer denoting search depth.
