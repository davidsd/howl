{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE UndecidableInstances  #-}

module MicroMath.ToBuiltin
  ( ToBuiltin(..)
  , builtinDecl
  ) where

import Data.Sequence          (Seq, pattern (:<|), pattern Empty)
import MicroMath.Eval.Context (Decl (..), Eval (..), Rule (..))
import MicroMath.Expr         (Expr (..), FromExpr (..), ToExpr (..),
                               pattern (:@))
import MicroMath.Symbol       (Symbol)

-- | Given a function type f, turn it into an operation on a 'Seq
-- Expr' (which we think of as the arguments to a symbolic function).
--
class ToBuiltin f where
  toBuiltin :: f -> (Seq Expr -> Eval (Maybe Expr))

-- | We provide instances for return types that could reasonably be
-- turned into a rule. The return values can be:
--
-- - Eval (Maybe a) : A monadic function that might fail to match
--   its arguments (signified by Nothing).
--
-- - Eval a : A monadic function that always matches its
--   arguments.
--
-- - Maybe a: A pure function that might fail to match its
--   arguments.
--
-- - a : A pure function that always matches its arguments
--
-- Where a is any instance of ToExpr.
instance {-# OVERLAPPABLE #-} ToExpr a => ToBuiltin (Eval (Maybe a)) where
  toBuiltin f0 = \case
    Empty -> fmap (fmap toExpr) f0
    _     -> pure Nothing
instance {-# OVERLAPPABLE #-} ToExpr a => ToBuiltin (Maybe a) where
  toBuiltin = toBuiltin @(Eval (Maybe Expr)) . pure . fmap toExpr
instance {-# OVERLAPPABLE #-} ToExpr a => ToBuiltin (Eval a) where
  toBuiltin = toBuiltin @(Eval (Maybe Expr)) . fmap (Just . toExpr)
instance {-# OVERLAPPABLE #-} ToExpr a => ToBuiltin a where
  toBuiltin = toBuiltin @(Eval (Maybe Expr)) . pure . Just . toExpr

-- | Provide instances for variadic functions with these return types,
-- i.e. functions that take a 'Seq Expr' as an argument. The
-- OVERLAPPING pragma instructs GHC to prefer these instances when
-- they apply.
instance {-# OVERLAPPING #-} ToExpr a => ToBuiltin (Seq Expr -> Eval (Maybe a)) where
  toBuiltin f = fmap (fmap toExpr) . f
instance {-# OVERLAPPING #-} ToExpr a => ToBuiltin (Seq Expr -> Eval a) where
  toBuiltin f = fmap (Just . toExpr) . f
instance {-# OVERLAPPING #-} ToExpr a => ToBuiltin (Seq Expr -> Maybe a) where
  toBuiltin f = pure . fmap toExpr . f
instance {-# OVERLAPPING #-} ToExpr a => ToBuiltin (Seq Expr -> a) where
  toBuiltin f = pure . Just . toExpr . f

-- | Provide instances for functions of the form a -> b -> ... -> r,
-- where r is one of the return types above, and a, b, ... are
-- instances of FromExpr.
--
-- Together, these instances allow us to "lift" most reasonable
-- Haskell functions to a function that can be used inside a rule.
-- Here are some example types that have instances for ToBuiltin:
--
-- Integer -> Double -> Text -> Bool
-- Symbol -> Maybe Double
-- Expr -> Integer -> Eval (Maybe Numeric)
-- ...
--
instance {-# OVERLAPPABLE #-} (FromExpr a, ToBuiltin f) => ToBuiltin (a -> f) where
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

builtinDecl :: ToBuiltin f => Symbol -> f -> Decl
builtinDecl sym f = builtinFunctionMaybeM sym (toBuiltin f)
