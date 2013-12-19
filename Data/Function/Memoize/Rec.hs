{-# LANGUAGE TemplateHaskell #-}
module Data.Function.Memoize.Rec ( memoizeRec )
  where

import Language.Haskell.TH
import Data.Function.Memoize.Class

memoizeRec :: Q [Dec] -> Q [Dec]
memoizeRec q = do ds <- q
                  mapM memoizeRec' ds

memoizeRec' :: Dec -> Q Dec
memoizeRec' (FunD f cs)         = do
  f' <- newName "f'"
  e <- [| memoize $(varE f') |]
  return $ ValD (VarP f) (NormalB e) [FunD f' cs]

memoizeRec' (ValD (VarP f) b w) = do
  f' <- newName "f'"
  e <- [| memoize $(varE f') |]
  return $ ValD (VarP f) (NormalB e) [ValD (VarP f') b w]

memoizeRec' d                   = return d
