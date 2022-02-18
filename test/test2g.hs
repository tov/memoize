{-# language DeriveAnyClass, DeriveGeneric #-}

import Data.Function.Memoize
import GHC.Generics (Generic)

data List a = Nil | Cons a (List a)
  deriving (Generic, Memoizable)

main = print $
  let lcs = memoFix2 -- exponential time if you put   fix   here
          $ \ f -> \ a b -> case (a,b) of
            (Cons x a', Cons y b') ->
               maximum [ if x == y then 1 + f a' b'  else 0, f a b', f a' b ]
            _ -> 0
      a = iterate (Cons ()) Nil !! 20
  in  lcs a a
