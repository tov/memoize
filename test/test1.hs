import Data.Function.Memoize
import Data.Function (fix) -- for comparison

main = print $
  let fib :: Integer -> Integer
      fib = memoFix $ \ f -> \ x -> if x < 2 then x else f (x-1) + f (x-2)
      -- and it would take much longer with  fib = fix $ \ f -> ...
  in  take 100 $ map fib [0..]
