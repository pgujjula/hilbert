Hilbert is a library for mathematics in Haskell. Right now the focus
is on number theory algorithms. I use this library to help me solve
Project Euler problems (www.projecteuler.net). I have implemented the
following algorithms and structures:
  
  * Continued fractions
  * Handling digits of integers
  * Legendre and Jacobi symbols
  * Handling Haskell lists
  * Modular exponentiation
  * Miller-Rabin primality test
  * Priority queues
  * Computing whether a number is a square

This project is still in a very early stage and has many bugs. I will
be expanding documentation, benchmarks, and tests. To build and try out
the project, clone the repo:

$ git clone https://www.github.com/pgujjula/hilbert
$ cd hilbert

Install dependencies:

$ cabal sandbox init
$ cabal install --only-dependencies

Configure and build:

$ cabal configure
$ cabal build

Run the tests if you like:

$ runhaskell -isrc -itest tests/Spec.hs

To install globally, remove the sandbox first:

$ rm -rfv .cabal-sandbox
$ cabal install

To build the documentation, use

$ cabal haddock --hyperlink-source

Report any problems or comments to preetham.gujjula@gmail.com
