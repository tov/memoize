{-# language DeriveGeneric, GADTs, StandaloneDeriving #-}

import Data.Function.Memoize
import Control.Monad (forM_, when)
import Test3Helper
import GHC.Generics (Generic)

-- NonstandardParams is defined by:
--
--   data NonstandardParams a b
--     = NonstandardParams (a -> Bool) b
--
-- This won’t compile because it needs addition typeclass constraints in
-- the instance context:
--
--   $(deriveMemoizable ''NonstandardParams)

deriving instance Generic (NonstandardParams a b)
instance (Eq a, Enum a, Bounded a, Memoizable b) => Memoizable (NonstandardParams a b)

applyToLength :: NonstandardParams Bool Int -> Bool
applyToLength (NonstandardParams f z) = f (odd z)

cases = [ (NonstandardParams id 5, True)
        , (NonstandardParams id 6, False)
        , (NonstandardParams not 5, False)
        , (NonstandardParams not 6, True)
        ]

main :: IO ()
main = do
  let memoized = memoize applyToLength
  forM_ cases $ \(input, expected) -> do
    let actual = applyToLength input
    when (actual /= expected) $
      fail $ "Test failed: got " ++ show actual ++
             " when " ++ show expected ++ " expected."
