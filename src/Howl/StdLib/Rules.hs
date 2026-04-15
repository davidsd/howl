{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Howl.StdLib.Rules
  ( addRulesBuiltins
  ) where

import Data.Sequence        (Seq, pattern (:<|), pattern Empty)
import Data.String          (fromString)
import Howl.Eval            (eval, tryApplyRule)
import Howl.Eval.Context    (Eval, HoldType (..), Rule (..), compilePat,
                             modifyAttributes, setHoldType)
import Howl.Expr            (Expr (..), FromExpr (..), pattern (:@),
                             pattern And)
import Howl.Expr            qualified as Expr
import Howl.Expr.TH         (declareBuiltins)
import Howl.StdLib.Types    (ListOrSolo (..))
import Howl.ToBuiltin       (Variadic (..), def)
import Howl.Util            (pattern Pair)

---------- ReplaceAll ----------

$(declareBuiltins ''Expr 'fromString
  [ "RuleDelayed"
  , "Rule"
  ])

-- | Either a Rule or RuleDelayed. These are treated in exactly the
-- same way -- the only difference being that RuleDelayed has
-- attribute HoldAll, while Rule has attribute HoldFirst. (In
-- Mathematica, Rule and RuleDelayed do not hold their first
-- arguments, but we differ here to avoid evaluating patterns.)
data ARule = MkRule Expr Expr

instance FromExpr ARule where
  fromExpr = \case
    Rule        :@ Pair lhs rhs -> Just $ MkRule lhs rhs
    RuleDelayed :@ Pair lhs rhs -> Just $ MkRule lhs rhs
    _ -> Nothing

-- | NB: replaceAll can currently be used to subvert shadowing
-- variables by replacing them with arbitrary expressions. For
-- example:
--
-- > bar = Function[x,x+2]
-- > bar /. x:>10
-- >>> Function[10,10+2]
--
-- Mathematica also has this problem. TODO: Should we fix it? The fix
-- probably involves using applySubstitutionsWithShadowing inside Eval
-- instead of applySubstitutions. Note that a workaround is to use
-- anonymous functions:
--
-- > bar = (#+2 &)
-- > bar /. x:>10
-- >>> Function[Slot[1]+2]
--
replaceAll :: Expr -> ListOrSolo ARule -> Eval Expr
replaceAll e (MkListOrSolo rules) = do
  rules' <- traverse aruleToRule rules
  go rules' e
  where
    aruleToRule :: ARule -> Eval Rule
    aruleToRule (MkRule lhs rhs) = do
      pat <- compilePat lhs
      pure $ PatRule pat rhs

    -- Repeatedly try rules in the given Sequence until one of them
    -- works
    tryRules _ Empty = pure Nothing
    tryRules expr (r :<| rs) = tryApplyRule r expr >>= \case
      result@(Just _) -> pure result
      Nothing         -> tryRules expr rs

    go :: Seq Rule -> Expr -> Eval Expr
    go rules' expr = tryRules expr rules' >>= \case
      Just result -> pure result
      -- If none of the rules work, go for the head and children
      Nothing -> case expr of
        h :@ cs -> do
          h' <- go rules' h
          cs' <- traverse (go rules') cs
          pure $ h' :@ cs'
        _       -> pure expr

---------- ReplaceRepeated ----------

replaceRepeated :: Expr -> ListOrSolo ARule -> Eval Expr
replaceRepeated expr rules = go expr
  where
    go currentExpr = do
      newExpr <- replaceAll currentExpr rules >>= eval
      if newExpr /= currentExpr
        then go newExpr
        else pure newExpr

---------- ConfirmPatternTest ----------

confirmPatternTest :: Seq Expr -> Maybe Expr
confirmPatternTest = \case
  Empty         -> Nothing
  test :<| rest -> Just $ And :@ fmap (Expr.unary test) rest

addRulesBuiltins :: Eval ()
addRulesBuiltins = do
  modifyAttributes "Rule"        (setHoldType HoldFirst)
  modifyAttributes "RuleDelayed" (setHoldType HoldAll)
  def "ReplaceAll" replaceAll
  def "ReplaceRepeated" replaceRepeated
  def "ConfirmPatternTest" (MkVariadic confirmPatternTest)
