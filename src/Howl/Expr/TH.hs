{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Howl.Expr.TH
  ( declareBuiltin
  , declareBuiltins
  ) where

import Language.Haskell.TH
import Data.Char (toLower)

-- | Generate a cached symbol CAF + a bidirectional pattern
-- synonym. These are convenient for pattern matching, and they also
-- ensure that we don't need to call fromString in the middle of
-- matching a pattern, which would obviate the advantage of having
-- O(1) equality checks for Symbols.
--
-- It is recommended to use declareBuiltin instead of fromString
-- whenever a symbol will be involved in evaluation, either in pattern
-- matching, or returned from a function.
--
-- Produces (for patStr="Plus", txt="Plus"):
--
--   {-# NOINLINE plusExpr #-}
--   plusExpr :: Expr
--   plusExpr = mkExpr "Plus"
--
--   isPlus :: Expr -> Bool
--   isPlus s = s == plusExpr
--
--   {-# INLINE Plus #-}  
--   pattern Plus :: Expr
--   pattern Plus <- (isPlus -> True) where
--     Plus = plusExpr
--
--   {-# COMPLETE Plus #-}
--
declareBuiltin
  :: Name   -- ^ Expr type, e.g. ''Expr
  -> Name   -- ^ mkExpr function, e.g. 'mkExpr
  -> String -- ^ pattern name, e.g. "Plus"
  -> String -- ^ interned text, e.g. "Plus"
  -> Q [Dec]
declareBuiltin symTy mkExpr patStr txt = do
  let patN     = mkName patStr
      varBase  = lowerFirst patStr
      symN     = mkName (varBase <> "Expr")    -- plusExpr
      predN    = mkName ("is" <> patStr)       -- isPlus

  -- plusExpr :: Expr
  symSig <- sigD symN (conT symTy)

  -- plusExpr = mkExpr "Plus"
  symVal <- valD (varP symN)
                 (normalB (varE mkExpr `appE` litE (stringL txt)))
                 []

  -- {-# NOINLINE plusExpr #-}
  symNoInline <- pragInlD symN NoInline FunLike AllPhases

  -- isPlus :: Expr -> Bool
  -- (Expr -> Bool) = (->) Expr Bool
  predSig <- sigD predN (appT (appT arrowT (conT symTy)) (conT ''Bool))

  -- isPlus s = s == plusExpr
  s <- newName "s"
  predVal <- funD predN
    [ clause [varP s]
        (normalB (infixE (Just (varE s)) (varE '(==)) (Just (varE symN))))
        []
    ]

  -- pattern Plus :: Expr
  patSig <- patSynSigD patN (conT symTy)

  -- {-# INLINE Plus #-}
  patInline <- pragInlD patN Inline FunLike AllPhases

  -- pattern Plus <- (isPlus -> True) where Plus = plusExpr
  let
    matcher = viewP (varE predN) (conP 'True [])
    builder = [clause [] (normalB (varE symN)) []]
  patDec <- patSynD patN
                   (prefixPatSyn [])
                   (explBidir builder)
                   matcher

  pure
    [ symNoInline
    , symSig
    , symVal
    , predSig
    , predVal
    , patSig
    , patInline
    , patDec
    ]

lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (c:cs) = toLower c : cs

-- | Generate many builtins
declareBuiltins
  :: Name       -- ^ ''Expr
  -> Name       -- ^ 'mkExpr
  -> [String]   -- ^ ["Plus","Times","power",...]
  -> Q [Dec]
declareBuiltins exprTy mkExpr =
  fmap concat . mapM (\s -> declareBuiltin exprTy mkExpr s s)
