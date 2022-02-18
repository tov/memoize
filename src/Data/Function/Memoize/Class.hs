{-# LANGUAGE 
      DefaultSignatures,
      EmptyCase,
      FlexibleContexts,
      LambdaCase,
      TypeOperators,
      UnicodeSyntax
    #-}
{- |
  The 'Memoizable' type class.
-}
module Data.Function.Memoize.Class (
  Memoizable(..)
) where

import GHC.Generics

-- | A memoization class.  An instance @'Memoizable' T@ for some
--   type @T@ means that that 'memoize' method can memoize for
--   parameters of type @T@.
class Memoizable a where
  memoize ∷ (a → v) → a → v
  default memoize :: (Generic a, GMemoizable (Rep a)) => (a → v) → a → v
  memoize f = gMemoize (f . to) . from

class GMemoizable a where
  gMemoize :: (a p → v) → a p → v

instance GMemoizable f => GMemoizable (M1 i c f) where
  gMemoize f = gMemoize (f . M1) . unM1

instance GMemoizable V1 where
  gMemoize _f = \case

instance GMemoizable U1 where
  gMemoize f =
    let fu = f U1
     in \U1 → fu

instance Memoizable c => GMemoizable (K1 i c) where
  gMemoize f = memoize (f . K1) . unK1

instance (GMemoizable a, GMemoizable b) => GMemoizable (a :*: b) where
  gMemoize f =
    let f' = gMemoize (\x → gMemoize (\y → f (x :*: y)))
     in \(x :*: y) → f' x y

instance (GMemoizable a, GMemoizable b) => GMemoizable (a :+: b) where
  gMemoize f =
    let fL = gMemoize (f . L1)
        fR = gMemoize (f . R1)
     in \case
          L1 x → fL x
          R1 x → fR x
