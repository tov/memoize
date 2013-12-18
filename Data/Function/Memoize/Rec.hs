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
  let body = LetE [FunD f' cs] e
  return $ ValD (VarP f) (NormalB body) []

memoizeRec' (ValD (VarP f) b w) = do
  f' <- newName "f'"
  e <- [| memoize $(varE f') |]
  let body = LetE [ValD (VarP f') b w] e
  return $ ValD (VarP f) (NormalB body) w

memoizeRec' d                   = return d
