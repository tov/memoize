{-# LANGUAGE
      TemplateHaskell,
      UnicodeSyntax,
      CPP
    #-}
{- |
    Exports functions for deriving instances of 'Memoizable' using
    Template Haskell.  The @TemplateHaskell@ language extension must be
    enabled to use the functions exported from this module.
-}
module Data.Function.Memoize.TH (
  deriveMemoizable, deriveMemoizableParams, deriveMemoize,
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Monad
import Language.Haskell.TH
import Data.Function.Memoize.Class

---
--- `#DEFINE`S FOR VERSION COMPATIBILITY
---

-- GHC 7.6 changed to StarT from StarK:
#if __GLASGOW_HASKELL__ >= 706
#  define COMPAT_STAR StarT
#else
#  define COMPAT_STAR StarK
#endif

--- TH 2.10 treats type classes like type constructors:
#if MIN_VERSION_template_haskell(2,10,0)
#  define COMPAT_CLASS_PRED(C)  (appT (conT (C)) . varT)
#else
#  define COMPAT_CLASS_PRED(C)  (classP (C) . (:[]) . varT)
#endif

-- TH 2.11 supports GADTs and adds a field to NewtypeD and DataD:
#if MIN_VERSION_template_haskell(2,11,0)
#  define COMPAT_TH_GADTS
#  define COMPAT_NEWTYPE_D(N, T, C)  (NewtypeD _ (N) (T) _ (C) _)
#  define COMPAT_DATA_D(N, T, C)     (DataD _ (N) (T) _ (C) _)
#else
#  undef  COMPAT_TH_GADTS
#  define COMPAT_NEWTYPE_D(N, T, C)  (NewtypeD _ (N) (T) (C) _)
#  define COMPAT_DATA_D(N, T, C)     (DataD _ (N) (T) (C) _)
#endif

-- GHC 9 adds a type parameter to the TyVarBndr type:
#if __GLASGOW_HASKELL__ >= 900
#  define COMPAT_TY_VAR_BNDR(V)      (TyVarBndr (V))
#  define COMPAT_PLAIN_TV(N)         (PlainTV (N) _)
#  define COMPAT_KINDED_TV(N, K)     (KindedTV (N) _ (K))
#else
#  define COMPAT_TY_VAR_BNDR(V)      TyVarBndr
#  define COMPAT_PLAIN_TV(N)         (PlainTV (N))
#  define COMPAT_KINDED_TV(N, K)     (KindedTV (N) (K))
#endif

-- |
-- To derive 'Memoizable' instances for the given data types.
-- In the simplest usage, to derive 'Memoizable' for an algebraic
-- datatype named @T@, write:
--
-- @
--   deriveMemoizable ''T
-- @
--
-- This assumes that all the type parameters of @T@ that are not
-- annotated with a kind other than @*@ should be listed as requiring
-- 'Memoizable' instances in the instance context.  For example, given
-- a data type declared as
--
-- @
--   data T a (b :: * -> *) c = ...
-- @
--
-- the generated instance will look like
--
-- @
--   instance ('Memoizable' a, 'Memoizable' c) =>
--            'Memoizable' (T a b c) where ...
-- @
--
-- For more precise control over the context, use
-- 'deriveMemoizableParams'.
--
-- N.B.: The @TemplateHaskell@ language extension must be enabled to use
-- this function.
deriveMemoizable ∷ Name → Q [Dec]
deriveMemoizable n = deriveMemoizable' n Nothing

-- |
-- Like 'deriveMemoizable' but takes a second argument, which is a list
-- of 'Int's to specify which type parameters of the type should be
-- mentioned in the context.  For example, given the same definition for
-- @T@ as above, we can write
--
-- @
--    deriveMemoizableParams ''T [3]
-- @
--
-- to leave the first parameter of @T@ out of the context and show
-- only the third, yielding the instance
--
-- @
--   instance 'Memoizable' c => 'Memoizable' (T a b c) where ...
-- @
--
-- N.B.: The @TemplateHaskell@ language extension must be enabled to use
-- this function.
deriveMemoizableParams ∷ Name → [Int] → Q [Dec]
deriveMemoizableParams n indices = deriveMemoizable' n (Just indices)

-- | In cases where neither 'deriveMemoizable' nor
-- 'deriveMemoizableParams' can figure out the right context for an
-- instance declaration, one can declare the instance manually and use
-- this function to derive the method body for 'memoize'. For example,
-- suppose that a data type @T@ is defined as:
--
-- @
--   data T a b = T (a -> Bool) b
-- @
--
-- For @T a b@ to be memoizable, @a -> Bool@ must be, and based on the
-- instance for '(->)', this means that @a@ must satisfy
-- 'Bounded' and 'Enum', so 'deriveMemoizable' cannot build the right
-- context for the 'Memoizable' instance.  Instead, one can write:
--
-- @
--   instance ('Eq' a, 'Enum' a, 'Bounded' a, 'Memoizable' b) =>
--            'Memoizable' (T a b) where
--     memoize = $(deriveMemoize ''T)
-- @
deriveMemoize ∷ Name → ExpQ
deriveMemoize name0 = do
  (_, _, cons) ← checkName name0
  buildMethodExp cons

-- | The main entry point delegates to check given type name, renames type
--   parameters, and generates the instance.
deriveMemoizable' ∷ Name → Maybe [Int] → Q [Dec]
deriveMemoizable' name0 mindices = do
  (name, tvbs, cons) ← checkName name0
  let tvs = freshNames tvbs
  inst ← instanceD
           (buildContext mindices tvbs tvs)
           (buildHead name tvs)
           [buildMethodDec cons]
  return [inst]

-- | Given the type name for the requested instance, checks if it
--   corresponds to a @data@ or @newtype@, and if so, returns the name,
--   a list of its parameters, and a list of constructor names with
--   their arities.
checkName ∷ Name → Q (Name, [COMPAT_TY_VAR_BNDR(())], [(Name, Int)])
checkName name0 = do
  let can'tDerive      = "deriveMemoizable: Can’t derive a Memoizable " ++
                         "instance for ‘" ++ show name0 ++ "’ because "
      can'tDeriveNonTC = can'tDerive ++ "it isn’t a type constructor."
      can'tDeriveGadt  = can'tDerive ++ "GADTs aren’t supported."
      --
      stdizeCon (NormalC name params) = return (name, length params)
      stdizeCon (RecC name fields)    = return (name, length fields)
      stdizeCon (InfixC _ name _)     = return (name, 2)
      stdizeCon (ForallC _ _ con)     = stdizeCon con
#ifdef COMPAT_TH_GADTS
      stdizeCon (GadtC _ _ _)         = fail can'tDeriveGadt
      stdizeCon (RecGadtC _ _ _)      = fail can'tDeriveGadt
#endif
  --
  info ← reify name0
  case info of
    TyConI (COMPAT_DATA_D(name, tvbs, cons)) → do
      conInfos ← mapM stdizeCon cons
      return (name, tvbs, conInfos)
    TyConI (COMPAT_NEWTYPE_D(name, tvbs, con)) → do
      conInfo ← stdizeCon con
      return (name, tvbs, [conInfo])
    _ → fail can'tDeriveNonTC

-- | Given a list, produces a list of nicely printable, distinct names.
--   Used so that instances print with nice parameters names, like
--
-- @
--    instance Memoizable (T a b c) where
-- @
--
-- instead of
--
-- @
--    instance Memoizable (T a[1] b[2] c32424534) where
-- @
freshNames ∷ [a] → [Name]
freshNames xs = take (length xs) alphabet
  where
  alphabet = [ mkName (c:s)
             | s ← "" : (show <$> [1 ∷ Integer ..])
             , c ← ['a' .. 'z'] ]

-- | Build the type class instance context, give the necessary
-- information to select which parameters to include.  If the first
-- argument is @Just ixs@, then there should be 'Memoizable' instances
-- for exactly those parameters, by index, in the context. Otherwise,
-- choose the parameters that have no explicit kind from the
-- list of binders. The third argument gives the actual type variable
-- names to use.
buildContext ∷ Maybe [Int] → [COMPAT_TY_VAR_BNDR(a)] → [Name] → CxtQ
buildContext mindices tvbs tvs =
  cxt (COMPAT_CLASS_PRED(''Memoizable) <$> cxttvs)
  where
  cxttvs = case mindices of
    Just ixs → filterBy (`elem` ixs) [1 ..] tvs
    Nothing  → filterBy isStar       tvbs   tvs
  --
  isStar (COMPAT_PLAIN_TV(_))               = True
  isStar (COMPAT_KINDED_TV(_, COMPAT_STAR)) = True
  isStar _                                  = False
  --
  filterBy ∷ (a → Bool) → [a] → [b] → [b]
  filterBy p xs ys = snd <$> filter (p . fst) (zip xs ys)

-- | Build the 'Memoizable' instance head for the given type name
--   and parameter type variables.
buildHead ∷ Name → [Name] → TypeQ
buildHead name tvs = 
  appT (conT ''Memoizable) (foldl appT (conT name) (varT <$> tvs))

-- | Build the 'memoize' method. The form of 'memoize' is always
--
-- @
--      memoize f = lookup where
--        cache1 = memoize $ \x1 -> ... memoize $ \x(a1) -> f (C1 x1 ...)
--        ...
--        cacheN = memoize $ \x1 -> ... memoize $ \x(aN) -> f (CN x1 ...)
--        lookup (C1 x1 ...) = cache1 x1 ...
--        ...
--        lookup (CN xN ...) = cacheN xN ...
-- @
--
-- where @C1@ ... @CN@ are the constructors of the data type and
-- @aj@ is the arity of constructor @Cj@.
--
-- In this method, we allocate fresh names for the parameter @f@, the
-- lookup function, and the @N@ caches.  We then delegate to build
-- the definitions of @look@ and the caches.
buildMethodDec ∷ [(Name, Int)] → DecQ
buildMethodDec cons = do
  valD (varP 'memoize)
    (normalB (buildMethodExp cons))
    []

-- | Build the body of the 'memoize' method, as described in the comment
-- above 'buildMethodDec'
buildMethodExp ∷ [(Name, Int)] → ExpQ
buildMethodExp cons = do
  f      ← newName "fun"
  caches ← mapM (\_ → newName "cache") cons
  lam1E (varP f)
    (letE
      (zipWith (buildCache f) cons caches)
      (buildLookup cons caches))

-- | Build the look function by building a clause for each constructor
--   of the datatype.
buildLookup ∷ [(Name, Int)] → [Name] → ExpQ
buildLookup cons caches = do
  a ← newName "arg"
  lam1E (varP a) .
    caseE (varE a) $
      zipWith buildLookupMatch cons caches

-- | Build a lookup clause for one constructor.  We lookup a value
--   by matching that constructor and then passing its parameters to
--   the cache for that constructor.
buildLookupMatch ∷ (Name, Int) → Name → MatchQ
buildLookupMatch (con, arity) cache = do
  params ← replicateM arity (newName "param")
  match (conP con (varP <$> params))
        (normalB (foldl appE (varE cache) (varE <$> params)))
        []

-- | Build the definition of a cache for the given constructor.  We do
--   this by binding the cache name to a cascading sequence of
--   memoizations for each component in the constructor's arity.
buildCache ∷ Name → (Name, Int) → Name → DecQ
buildCache f (con, arity) cache =
  valD (varP cache) (normalB (composeMemos arity f (conE con))) []

-- | Given the remaining arity to memoize, the name of the function to
--   memoize, and the accumulated parameter so far, build the
--   memoization chain.
composeMemos ∷ Int → Name → ExpQ → ExpQ
composeMemos 0     f arg = [| $(varE f) $arg |]
composeMemos arity f arg = do
  [| memoize $ \b → $(composeMemos (arity - 1) f [| $arg b |]) |]

