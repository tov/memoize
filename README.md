memoize
=======

[![Build Status](https://travis-ci.org/tov/memoize.svg)](http://travis-ci.org/tov/memoize)

This library provides a type class `Memoizable` for memoizing
functions, along with instances for a variety of argument types.
It includes a Template Haskell function for deriving
`Memoizable` instances for arbitrary algebraic datatypes.

The library constructs pure memo caches without the use of
`unsafePerformIO`.  This technique relies on implementation
assumptions that, while not guaranteed by the semantics of
Haskell, appear to be true.
