{-# LANGUAGE
      TemplateHaskell,
      UnicodeSyntax
    #-}
{- |
    Exports functions for deriving instances of 'Memoizable' using
    Template Haskell.  The @TemplateHaskell@ language extension must be
    enabled to use the functions exported from this module.
-}
module Data.Function.Memoize.TH (
  deriveMemoizable, deriveMemoizableParams, deriveMemoize,
) where

import Control.Applicative
import Control.Monad
import Language.Haskell.TH

import Data.Function.Memoize.Class

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
--   instance ('Enum' a, 'Bounded' a, 'Memoizable' b) =>
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
checkName ∷ Name → Q (Name, [TyVarBndr], [(Name, Int)])
checkName name0 = do
  info            ← reify name0
  case info of
    TyConI (DataD _ name tvbs cons _)
               → return (name, tvbs, stdizeCon <$> cons)
    TyConI (NewtypeD _ name tvbs con _)
               → return (name, tvbs, [stdizeCon con])
    _          → fail $
      "deriveMemoizable: Can't derive a Memoizable instance for `" ++
      show name0 ++ "' because it isn't a type constructor."
  where
    stdizeCon (NormalC name params) = (name, length params)
    stdizeCon (RecC name fields)    = (name, length fields)
    stdizeCon (InfixC _ name _)     = (name, 2)
    stdizeCon (ForallC _ _ con)     = stdizeCon con

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
-- choose the parameters that have no explicit kind or kind @*@ from the
-- list of binders. The third argument gives the actual type variable
-- names to use.
buildContext ∷ Maybe [Int] → [TyVarBndr] → [Name] → CxtQ
buildContext mindices tvbs tvs =
  cxt (classP ''Memoizable . (:[]) . varT <$> cxttvs)
  where
  cxttvs = case mindices of
    Just ixs → filterBy (`elem` ixs) [1 ..] tvs
    Nothing  → filterBy isStar       tvbs   tvs
  --
  isStar (PlainTV _) = True
  isStar (KindedTV _ StarK) = True
  isStar (KindedTV _ _) = False
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
  f      ← newName "f"
  look   ← newName "look"
  caches ← mapM (newName . ("cache"++) . nameBase . fst) cons
  lam1E (varP f)
    (letE
      (buildLookup look cons caches
        : zipWith (buildCache f) cons caches)
      (varE look))

-- | Build the look function by building a clause for each constructor
--   of the datatype.
buildLookup ∷ Name → [(Name, Int)] → [Name] → DecQ
buildLookup look cons caches =
  funD look (zipWith buildLookupClause cons caches)

-- | Build a lookup clause for one constructor.  We lookup a value
--   by matching that constructor and then passing its parameters to
--   the cache for that constructor.
buildLookupClause ∷ (Name, Int) → Name → ClauseQ
buildLookupClause (con, arity) cache = do
  params ← replicateM arity (newName "a")
  clause [conP con (varP <$> params)]
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
  [| memoize $ \b -> $(composeMemos (arity - 1) f [| $arg b |]) |]

