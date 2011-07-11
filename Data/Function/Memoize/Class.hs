{-# LANGUAGE
      UnicodeSyntax
    #-}
{- |
  The 'Memoizable' type class.
-}
module Data.Function.Memoize.Class (
  Memoizable(..)
) where

-- | A memoization class.  An instance @'Memoizable' T@ for some
--   type @T@ means that that 'memoize' method can memoize for
--   parameters of type @T@.
class Memoizable a where
  memoize ∷ (a → v) → a → v
