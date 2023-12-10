# mult3plus1

Try running `stack install` and then `mult3plus1 N`, where N is a
positive integer.  This program will try to find the fraction of all
integers greater than 1 modulo $2^{N+1}$ that are guaranteed to become
smaller when run through the calculation of the [the Collatz
conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture).  The idea
is that if all these integers can be shown to have this property
(unlikely), then they will all end up at the integer 1 at the end, and
the conjecture is proven.


## Background

I tried to prove the conjecture in July 2019 by programming my way out
of it, but I didn't succeed.

