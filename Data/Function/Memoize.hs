{-# LANGUAGE
      DeriveFunctor,
      GeneralizedNewtypeDeriving,
      TemplateHaskell,
      UnicodeSyntax
  #-}
{- |
  A function memoization library.

  This includes a class for memoizable argument types and a Template
  Haskell expander for deriving instances of the class.

  Note that most memoization in this style relies on assumptions about
  the implementation of non-strictness (as laziness) that are not
  guaranteed by the semantics. However, it appears to work.
-}
module Data.Function.Memoize (
  -- * Memoization class
  Memoizable(..),
  -- ** Operations
  -- *** Higher-arity memoize
  memoize2, memoize3, memoize4, memoize5, memoize6, memoize7,
  -- *** Memoizing open recursion
  memoFix, memoFix2, memoFix3, memoFix4, memoFix5, memoFix6, memoFix7,
  -- *** Tracing memoization
  traceMemoize,

  -- * For making instances for finite types
  memoizeFinite,

  -- * Deriving 'Memoizable'
  deriveMemoizable, deriveMemoizableParams, deriveMemoize,
) where

import Control.Applicative
import Control.Monad
import Debug.Trace

import Data.Function.Memoize.Class
import Data.Function.Memoize.TH

-- | Memoize a two argument function
memoize2 ∷ (Memoizable a, Memoizable b) ⇒
           (a → b → v) → a → b → v
memoize2 v = memoize (memoize . v)

-- | Memoize a three argument function
memoize3 ∷ (Memoizable a, Memoizable b, Memoizable c) ⇒
           (a → b → c → v) → a → b → c → v
memoize3 v = memoize (memoize2 . v)

-- | Memoize a four argument function
memoize4 ∷ (Memoizable a, Memoizable b, Memoizable c, Memoizable d) ⇒
           (a → b → c → d → v) →
           a → b → c → d → v
memoize4 v = memoize (memoize3 . v)

-- | Memoize a five argument function
memoize5 ∷ (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
            Memoizable e) ⇒
           (a → b → c → d → e → v) →
           a → b → c → d → e → v
memoize5 v = memoize (memoize4 . v)

-- | Memoize a six argument function
memoize6 ∷ (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
            Memoizable e, Memoizable f) ⇒
           (a → b → c → d → e → f → v) →
           a → b → c → d → e → f → v
memoize6 v = memoize (memoize5 . v)

-- | Memoize a seven argument function
memoize7 ∷ (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
            Memoizable e, Memoizable f, Memoizable g) ⇒
           (a → b → c → d → e → f → g → v) →
           a → b → c → d → e → f → g → v
memoize7 v = memoize (memoize6 . v)

-- | Memoizes the least fixed point of a function. This is like
-- 'Data.Function.fix', but it passes the fixed function a memoized
-- version of itself, so this memoizes using all recursive calls as well.
memoFix ∷ Memoizable a ⇒ ((a → v) → a → v) → a → v
memoFix ff = f where f = memoize (ff f)

-- | Two argument version of 'memoFix'.
memoFix2 ∷ (Memoizable a, Memoizable b) ⇒
           ((a → b → v) → a → b → v) → a → b → v
memoFix2 ff = f where f = memoize2 (ff f)

-- | Three argument version of 'memoFix'.
memoFix3 ∷ (Memoizable a, Memoizable b, Memoizable c) ⇒
           ((a → b → c → v) → a → b → c → v) → a → b → c → v
memoFix3 ff = f where f = memoize3 (ff f)

-- | Four argument version of 'memoFix'.
memoFix4 ∷ (Memoizable a, Memoizable b, Memoizable c, Memoizable d) ⇒
           ((a → b → c → d → v) → (a → b → c → d → v)) →
           a → b → c → d → v
memoFix4 ff = f where f = memoize4 (ff f)

-- | Five argument version of 'memoFix'.
memoFix5 ∷ (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
            Memoizable e) ⇒
           ((a → b → c → d → e → v) → (a → b → c → d → e → v)) →
           a → b → c → d → e → v
memoFix5 ff = f where f = memoize5 (ff f)

-- | Six argument version of 'memoFix'.
memoFix6 ∷ (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
            Memoizable e, Memoizable f) ⇒
           ((a → b → c → d → e → f → v) → (a → b → c → d → e → f → v)) →
           a → b → c → d → e → f → v
memoFix6 ff = f where f = memoize6 (ff f)

-- | Seven argument version of 'memoFix'.
memoFix7 ∷ (Memoizable a, Memoizable b, Memoizable c, Memoizable d,
            Memoizable e, Memoizable f, Memoizable g) ⇒
           ((a → b → c → d → e → f → g → v) → (a → b → c → d → e → f → g → v)) →
           a → b → c → d → e → f → g → v
memoFix7 ff = f where f = memoize7 (ff f)

-- | Give a one-argument function whose argument satisfies 'Show',
--   this memoizes the function such that the argument is shown (using
--   'Debug.Trace.trace') only when the function has to be applied, as
--   opposed to when the answer is available in the memo cache.
traceMemoize ∷ (Memoizable a, Show a) ⇒ (a → b) → a → b
traceMemoize f = memoize (\a → traceShow a (f a))

---
--- Derived instances
---

deriveMemoizable ''()
deriveMemoizable ''Bool
deriveMemoizable ''Ordering
deriveMemoizable ''Maybe
deriveMemoizable ''Either
deriveMemoizable ''[]

deriveMemoizable ''(,)
deriveMemoizable ''(,,)
deriveMemoizable ''(,,,)
deriveMemoizable ''(,,,,)
deriveMemoizable ''(,,,,,)
deriveMemoizable ''(,,,,,,)
deriveMemoizable ''(,,,,,,,)
deriveMemoizable ''(,,,,,,,,)
deriveMemoizable ''(,,,,,,,,,)
deriveMemoizable ''(,,,,,,,,,,)
deriveMemoizable ''(,,,,,,,,,,,)

---
--- Binary-tree based memo caches
---

-- Used for both 'Integer' and arbitrary 'Int'-like types.

data BinaryTreeCache v
 = BinaryTreeCache {
    btValue         ∷ v,
    btLeft, btRight ∷ BinaryTreeCache v
   }
   deriving Functor

---
--- 'Integer' memoization
---

instance Memoizable Integer where
  memoize f = integerLookup (f <$> theIntegers)

-- | An integer cache stores a value for 0 and separate caches for the
--   positive and negative integers.
data IntegerCache v
  = IntegerCache {
      icZero                 ∷ v,
      icNegative, icPositive ∷ PosIntCache v
    }
  deriving Functor

-- | A positive integer cache is represented as a little-ending bitwise
--   trie
type PosIntCache v = BinaryTreeCache v

theIntegers ∷ IntegerCache Integer
theIntegers
  = IntegerCache {
      icZero     = 0,
      icNegative = negate <$> thePosInts,
      icPositive = thePosInts
    }

thePosInts ∷ PosIntCache Integer
thePosInts =
  BinaryTreeCache {
   btValue = 1,
   btLeft  = fmap (* 2) thePosInts,
   btRight = fmap (succ . (* 2)) thePosInts
 }

integerLookup ∷ IntegerCache v → Integer → v
integerLookup cache n =
  case n `compare` 0 of
    EQ → icZero cache
    GT → posIntLookup (icPositive cache) n
    LT → posIntLookup (icNegative cache) (negate n)

-- PRECONDITION: @n@ is a positive 'Integer'
posIntLookup ∷ PosIntCache v → Integer → v
posIntLookup cache 1 = btValue cache
posIntLookup cache n
  | even n    = posIntLookup (btLeft cache) (n `div` 2)
  | otherwise = posIntLookup (btRight cache) (n `div` 2)

---
--- Enumerable types using binary search trees
---

newtype Finite a = ToFinite { fromFinite ∷ a }
  deriving (Eq, Bounded, Enum)

instance (Bounded a, Enum a) ⇒ Memoizable (Finite a) where
  memoize f = finiteLookup (f <$> theFinites)

-- | For finite 'Int'-like types, we use a balanced binary search tree
--   indexed to every element from 'minBound' to 'maxBound'
theFinites ∷ (Bounded a, Enum a) ⇒ BinaryTreeCache a
theFinites = loop minBound maxBound where
  loop start stop =
    BinaryTreeCache {
      btValue = mean,
      btLeft  = loop start (pred mean),
      btRight = loop (succ mean) stop
    }
    where mean = meanFinite start stop

finiteLookup ∷ (Bounded a, Enum a) ⇒ BinaryTreeCache v → a → v
finiteLookup cache0 a0 =
  loop start0 stop0 cache0 where
    start0 = fromEnum (minBound `asTypeOf` a0)
    stop0  = fromEnum (maxBound `asTypeOf` a0)
    a      = fromEnum a0
    loop start stop cache =
      let mean = meanFinite start stop in
        case a `compare` mean of
          EQ → btValue cache
          LT → loop start (pred mean) (btLeft cache)
          GT → loop (succ mean) stop (btRight cache)

meanFinite     ∷ (Bounded a, Enum a) ⇒ a → a → a
meanFinite a b = toEnum (ia `div` 2 + ib `div` 2 +
                           if odd ia && odd ib then 1 else 0)
  where
    ia = fromEnum a
    ib = fromEnum b

-- | Can be used to memoize over any "finite" type satisfying
-- 'Enum' and 'Bounded'.  This builds a binary search tree, treating
-- the memoized type as isomorphic to a range of 'Int', so it will be
-- only as efficient as 'toEnum', 'fromEnum', 'succ', and 'pred'.
--
-- This can be used to make instances for finite types. For example, the
-- instances for 'Int' and 'Char' are declared as:
--
-- @
--   instance Memoizable Int where memoize = memoizeFinite
--   instance Memoizable Char where memoize = memoizeFinite
-- @
memoizeFinite   ∷ (Enum a, Bounded a) ⇒ (a → v) → a → v
memoizeFinite f = memoize (f . fromFinite) . ToFinite

instance Memoizable Int where memoize = memoizeFinite
instance Memoizable Char where memoize = memoizeFinite

---
--- Functions
---

instance (Eq a, Bounded a, Enum a, Memoizable b) ⇒ Memoizable (a → b) where
  memoize = functionLookup . theFunctions

functionLookup ∷ (Eq a, Bounded a, Enum a, Memoizable b) ⇒
                 FunctionCache b v → (a → b) → v
functionLookup cache f =
  fcNil (foldl fcCons cache (f <$> [minBound .. maxBound]))

theFunctions ∷ (Eq a, Bounded a, Enum a, Memoizable b) ⇒
               ((a → b) → v) → FunctionCache b v
theFunctions f =
  FunctionCache {
    fcNil  = f undefined,
    fcCons = memoize (\b → theFunctions (f . extend b))
  }
    where
      extend b g a
        | a == minBound = b
        | otherwise     = g (pred a)

data FunctionCache b v
  = FunctionCache {
      fcNil  ∷ v,
      fcCons ∷ b → FunctionCache b v
    }

---
--- Example functions
---

-- Memoize on 'Integer'. If memoization doesn't work, this will be
-- horribly slow.
_fib ∷ Integer → Integer
_fib = memoFix $ \fib n → case n of
  0 → 1
  1 → 1
  _ → fib (n - 1) + fib (n - 2)

-- Memoize on a function.  The use of 'trace' will indicate when
-- the function is called to fill in the memo cache.
_isNot       ∷ (Bool → Bool) → Bool
_isNot       = memoize $ \f →
  trace "_isNot" $
    f True == False && f False == True

-- Memoize on a curried function!
_countTrue ∷ (Bool → Bool → Bool) → Integer
_countTrue = memoize $ \f →
  trace "_countTrue" $
    toInteger (length (f <$> [False,True] <*> [False,True] >>= guard))

