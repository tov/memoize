# Change Log

## [1.1.2] - 2022-09-16
- fix severe slowness in `Memoizable Integer` instance (h/t @darrenks)
- correct minimum `base` version (h/t @Bodigrim)

## [1.1.1] - 2021-10-31
- add this changelog to `memoize.cabal`

## [1.1.0] - 2021-10-31
- add `Memoizable` instances for types from `base`:
   - `Data.Complex.Complex`
   - `Data.Ratio.Ratio`
   - `Data.Tuple.Solo`
   - `Data.Version.Version`
   - `Data.Void.Void`
- fix misspelling of “little-endian” in docs

## [1.0.0] - 2021-10-27
- support GHC 9

## [0.8.0] - 2016-09-03
- support GHC 8 and `template-haskell` 2.11.0

## [0.7.0] - 2015-03-30
- support GHC 7.10

## [0.6.0] - 2014-04-01
- support GHC 7.6

## [0.4.0] - 2014-04-01
- prevent use of `template-haskell` 3

## [0.3.0] - 2012-03-12
- support GHC 7.4

## [0.2.0] - 2012-03-06
- fix: arithmetic overflow in memoization on `Int`

## [0.1.0] - 2011-07-11
- initialize release
