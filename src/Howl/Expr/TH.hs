{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell helpers for generating cached symbol-expression
-- pattern synonyms.
module Howl.Expr.TH
  ( declareExprPattern
  , declareExprPatterns
  ) where

import Data.Char           (toLower)
import Language.Haskell.TH

-- | Generate a cached symbol CAF + a bidirectional pattern
-- synonym. These are convenient for pattern matching, and they also
-- ensure that we don't need to call fromString in the middle of
-- matching a pattern, which would obviate the advantage of having
-- O(1) equality checks for Symbols.
--
-- It is recommended to use declareExprPattern instead of fromString
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
declareExprPattern
  :: Name   -- ^ Expr type, e.g. ''Expr
  -> Name   -- ^ mkExpr function, e.g. 'mkExpr
  -> String -- ^ pattern name, e.g. "Plus"
  -> String -- ^ interned text, e.g. "Plus"
  -> Q [Dec]
declareExprPattern symTy mkExpr patStr txt = do
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

-- | Generate many expression patterns.
declareExprPatterns
  :: Name       -- ^ ''Expr
  -> Name       -- ^ 'mkExpr
  -> [String]   -- ^ ["Plus","Times","power",...]
  -> Q [Dec]
declareExprPatterns exprTy mkExpr =
  fmap concat . mapM (\s -> declareExprPattern exprTy mkExpr s s)
