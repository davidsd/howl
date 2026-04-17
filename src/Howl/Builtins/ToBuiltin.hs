{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Convert Haskell functions into builtin Howl rules.
module Howl.Builtins.ToBuiltin
  ( ToBuiltin(..)
  , Variadic(..)
  , def
  , builtinDecl
  ) where

import Data.Sequence     (Seq, pattern (:<|), pattern Empty)
import Howl.Eval.Context (Decl (..), Eval (..), Rule (..), addDecl)
import Howl.Expr         (Expr (..), FromExpr (..), ToExpr (..), pattern (:@))
import Howl.Symbol       (Symbol)

-- | Given a function type @f@, turn it into an operation on a
-- @Seq Expr@, interpreted as the arguments to a symbolic function.
--
-- We provide instances for return types that can reasonably be turned
-- into rules, where the final result type is any instance of
-- 'ToExpr'.
--
class ToBuiltin f where
  toBuiltin :: f -> (Seq Expr -> Eval (Maybe Expr))

-- | A monadic builtin that may fail to match its arguments, signified
-- by 'Nothing'.
instance {-# OVERLAPPABLE #-} ToExpr a => ToBuiltin (Eval (Maybe a)) where
  toBuiltin f0 = \case
    Empty -> fmap (fmap toExpr) f0
    _     -> pure Nothing
-- | A pure builtin that may fail to match its arguments, signified by
-- 'Nothing'.
instance {-# OVERLAPPABLE #-} ToExpr a => ToBuiltin (Maybe a) where
  toBuiltin = toBuiltin @(Eval (Maybe Expr)) . pure . fmap toExpr
-- | A monadic builtin that always matches its arguments.
instance {-# OVERLAPPABLE #-} ToExpr a => ToBuiltin (Eval a) where
  toBuiltin = toBuiltin @(Eval (Maybe Expr)) . fmap (Just . toExpr)
-- | A pure builtin that always matches its arguments.
instance {-# OVERLAPPABLE #-} ToExpr a => ToBuiltin a where
  toBuiltin = toBuiltin @(Eval (Maybe Expr)) . pure . Just . toExpr


-- | Mark a function as variadic, i.e. as taking a @Seq a@ of arbitrary
-- length as its argument list.
newtype Variadic a b = MkVariadic (Seq a -> b)

-- | A variadic monadic builtin that may fail to match its arguments,
-- signified by 'Nothing'.
instance {-# OVERLAPPABLE #-} (FromExpr a, ToExpr b) => ToBuiltin (Variadic a (Eval (Maybe b))) where
  toBuiltin (MkVariadic f) xs = case mapM fromExpr xs of
    Just as -> fmap (fmap toExpr) $ f as
    Nothing -> pure Nothing
-- | A variadic pure builtin that may fail to match its arguments,
-- signified by 'Nothing'.
instance {-# OVERLAPPABLE #-} (FromExpr a, ToExpr b) => ToBuiltin (Variadic a (Maybe b)) where
  toBuiltin (MkVariadic f) xs = case mapM fromExpr xs of
    Just as -> pure $ fmap toExpr $ f as
    Nothing -> pure Nothing
-- | A variadic monadic builtin that always matches its arguments.
instance {-# OVERLAPPABLE #-} (FromExpr a, ToExpr b) => ToBuiltin (Variadic a (Eval b)) where
  toBuiltin (MkVariadic f) xs = case mapM fromExpr xs of
    Just as -> fmap (Just . toExpr) $ f as
    Nothing -> pure Nothing
-- | A variadic pure builtin that always matches its arguments.
instance {-# OVERLAPPABLE #-} (FromExpr a, ToExpr b) => ToBuiltin (Variadic a b) where
  toBuiltin (MkVariadic f) xs = case mapM fromExpr xs of
    Just as -> pure . Just . toExpr $ f as
    Nothing -> pure Nothing

-- | Provide instances for functions of the form
-- @a -> b -> ... -> r@, where @r@ is one of the return types above
-- and @a@, @b@, ... are instances of 'FromExpr'.
--
-- Together, these instances allow us to "lift" most reasonable
-- Haskell functions to functions that can be used inside rules.
-- Here are some example types that have instances for 'ToBuiltin':
--
-- - @Integer -> Double -> Text -> Bool@
-- - @Symbol -> Maybe Double@
-- - @Expr -> Integer -> Eval (Maybe Numeric)@
--
instance (FromExpr a, ToBuiltin f) => ToBuiltin (a -> f) where
  toBuiltin f = \case
    x :<| rest -> case fromExpr x of
      Just x' -> toBuiltin (f x') rest
      Nothing -> pure Nothing
    _ -> pure Nothing

withHeadMaybeM :: Monad m => Expr -> (Seq Expr -> m (Maybe a)) -> Expr -> m (Maybe a)
withHeadMaybeM h f = \case
  h' :@ args | h == h' -> f args
  _                    -> pure Nothing

builtinFunctionMaybeM :: Symbol -> (Seq Expr -> Eval (Maybe Expr)) -> Decl
builtinFunctionMaybeM sym f = DownValue sym $ BuiltinRule $ withHeadMaybeM (ExprSymbol sym) f

-- | Define a builtin rule by converting a Haskell function into a Howl
-- definition using 'ToBuiltin'.
def :: ToBuiltin f => Symbol -> f -> Eval ()
def sym f = addDecl $ builtinDecl sym f

-- | Convert a Haskell function into a declaration for a builtin
-- downvalue.
builtinDecl :: ToBuiltin f => Symbol -> f -> Decl
builtinDecl sym f = builtinFunctionMaybeM sym (toBuiltin f)
