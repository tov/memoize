module Test3Helper where

data NonstandardParams a b = NonstandardParams (a -> Bool) b
