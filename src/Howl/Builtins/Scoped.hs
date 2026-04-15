{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Howl.Builtins.Scoped
  ( addScopedBuiltins
  ) where

import Data.Foldable           qualified as Foldable
import Data.Sequence           (Seq, pattern (:<|), pattern Empty)
import Data.Sequence           qualified as Seq
import Data.String             (fromString)
import Howl.Builtins.ToBuiltin (def)
import Howl.Builtins.Types     (ListOrSolo (..))
import Howl.Eval               (Substitution (..), SubstitutionSet,
                                emptySubstitutionSet, insertSubstitution,
                                insertSubstitutions, lookupBinding,
                                removeBindings, singletonSubstitutionSet)
import Howl.Eval.Context       (Decl (..), Eval, HoldType (..), Rule (..),
                                addDecl, modifyAttributes, newModuleSymbol,
                                setHoldType)
import Howl.Expr               (Expr (..), FromExpr (..), Numeric (..),
                                pattern (:@), pattern ExprInteger,
                                pattern ExprNumeric, pattern ExprSymbol,
                                pattern ExprView, pattern List, pattern Set,
                                pattern Slot, pattern SlotSequence)
import Howl.Expr               qualified as Expr
import Howl.Expr.TH            (declareExprPatterns)
import Howl.Symbol             (Symbol)
import Howl.Util               (pattern Pair, pattern Solo)

-- ========== Scoped constructs ========== --

$(declareExprPatterns ''Expr 'fromString
  [ "Function"
  , "Let"
  , "Module"
  , "Table"
  ])

data TableRange
  = RangeLength Integer
  | RangeIndices Symbol Numeric Numeric Numeric
  | RangeList Symbol (Seq Expr)

instance FromExpr TableRange where
  fromExpr = \case
    List :@ Solo (ExprInteger i)
      -> Just $ RangeLength i
    List :@ (ExprSymbol x :<| ExprNumeric i :<| Empty)
      -> Just $ RangeIndices x 1 i 1
    List :@ (ExprSymbol x :<| ExprNumeric i :<| ExprNumeric j :<| Empty)
      -> Just $ RangeIndices x i j 1
    List :@ (ExprSymbol x :<| ExprNumeric i :<| ExprNumeric j :<| ExprNumeric k :<| Empty)
      -> Just $ RangeIndices x i j k
    List :@ (ExprSymbol x :<| (List :@ xs) :<| Empty)
      -> Just $ RangeList x xs
    _ -> Nothing

-- | Detects any new variables introduced by the given
-- expression. This is used to avoid overwriting shadowing variables
-- in constructs like Function, Let, Module, and Table.
--
-- For example, we should have:
--
-- --> Let[x=2, Function[x, Function[x,x+9]][12][x]]
-- --> Function[x, Function[x,x+9]][12][2]
-- --> Function[x,x+9][2]
-- --> 2 + 9
-- --> 11
--
introducedVariables :: Expr -> Seq Symbol
introducedVariables = \case
  Function :@ Pair (ExprView (MkListOrSolo vars))     _ -> vars
  Let      :@ Pair (ExprView (MkListOrSolo bindings)) _ -> fmap bindVar bindings
  Module   :@ Pair (ExprView (MkListOrSolo vars))     _ -> vars
  Table    :@ Pair _ (ExprView range) -> case range of
    RangeIndices var _ _ _ -> Solo var
    RangeList var _        -> Solo var
    _                      -> Empty
  _ -> Empty

-- | Replace the Symbols in the given Expr with their corresponding
-- Bindings in 'substSet', allowing new local variables introduced in
-- sub-expressions to shadow the given substitutions.
--
-- [NB Shadowing in patterns]: It would be nice if we could implement
-- shadowing for variables in patterns and rules. For example, morally
-- speaking we should not be able to modify the variable x inside the
-- rule:
--
-- x_ :> x+1
--
-- However, both Mathematica and Howl currently have:
--
-- (x_:>x+2)/.x->12 ---> Pattern[12, Blank[]] :> 12 + 2
--
-- It is difficult to get around this because x is introduced in a
-- pattern and then used on the RHS of a rule, and we would have to
-- enumerate all the places that a variable bound in a pattern might
-- show up. Maybe it's only in Rule, RuleDelayed, Set, SetDelayed,
-- TagSetDelayed, UpSet, UpSetDelayed?
applySubstitutionsWithShadowing :: SubstitutionSet -> Expr -> Expr
applySubstitutionsWithShadowing = go
  where
    go substSet expr = case expr of
      ExprApp h cs ->
        let
          newSubstSet = removeBindings (introducedVariables expr) substSet
          h' = go newSubstSet h
          cs' = fmap (go newSubstSet) cs
        in
          ExprApp h' cs'
      ExprSymbol sym
        | Just rhs <- lookupBinding sym substSet -> rhs
      _ -> expr

---------- Function ----------

-- | Function[x,body][y] is implemented by performing the symbolic
-- substitution body /. x->y *before evaluation*. As a consequence,
-- any occurences of x inside body will be substituted with y,
-- regardless of whether the symbol x is bound to anything outside the
-- scope of the Function. In other words, function variables exhibit
-- shadowing.
--
-- Meanwhile Function[body] is an anonymous function, where body
-- contains some number of Slot[_]'s. We implement it by replacing
-- Slot[i] inside body with the appropriate argument, but not inside
-- other anonymous functions. This avoids a situation like this:
--
-- Function[Slot[1]*Function[1+Slot[1]]][y] --> y*Function[1+y] (BAD)
--
-- Instead, we should have:
--
-- Function[Slot[1]*Function[1+Slot[1]]][y] -> y*Function[1+Slot[1]]
--
-- NB: When we apply substitutions, we need to be careful not to do it
-- inside a construct that introduces variables.
--
functionDef :: Expr -> Maybe Expr
functionDef = \case
  (Function :@ Solo body) :@ args -> Just $ replaceSlots args body
  (Function :@ Pair (ExprView (MkListOrSolo vars)) body) :@ args
    | Just bindings <- bindVars_maybe vars args
    -> Just $ applySubstitutionsWithShadowing bindings body
  _ -> Nothing
  where
    bindVars_maybe :: Seq Symbol -> Seq Expr -> Maybe SubstitutionSet
    bindVars_maybe Empty      Empty      = Just emptySubstitutionSet
    bindVars_maybe (a :<| as) (b :<| bs) =
      insertSubstitution (MkSubstitution a b) =<< bindVars_maybe as bs
    bindVars_maybe _          _          = Nothing

    replaceSlots :: Seq Expr -> Expr -> Expr
    replaceSlots vals = go
      where
        go slotExpr@(Slot :@ Solo (ExprInteger i))
          | Just val <- Seq.lookup (fromInteger i - 1) vals = val
          | otherwise = slotExpr
        go (SlotSequence :@ Solo (ExprInteger i)) =
          Expr.Sequence :@ Seq.drop (fromIntegral i - 1) vals
        -- Important: do not replace inside an anonymous function!
        go expr@(Function :@ Solo _)      = expr
        go (h :@ args)                    = go h :@ fmap go args
        go expr                           = expr

-- | Note: functionDef is a curried definition: it does NOT match
-- something of the form Function[...]. Thus, we cannot use
-- 'builtinFunctionMaybeM' or 'function'. We need to construct the
-- Decl by hand.
functionDecl :: Decl
functionDecl = DownValue "Function" $ BuiltinRule (pure . functionDef)

---------- Let ----------

-- | A datatype for parsing a binding x=y, where x is a 'Symbol'
data SetBind = MkSetBind Symbol Expr

instance FromExpr SetBind where
  fromExpr = \case
    Set :@ Pair (ExprSymbol x) y -> Just $ MkSetBind x y
    _                            -> Nothing

bindVar :: SetBind -> Symbol
bindVar (MkSetBind x _) = x

-- | Let is a construct for creating local variables. It is different
-- from the scoping constructs in Mathematica. The variables in a Let
-- are locally defined, and they shadow global definitions. Subsequent
-- bindings can refer to previous bindings.
--
-- Let is defined by the following transformation:
--
-- Let[{}, expr]          ---> expr
-- Let[{x=x0,...}, expr]  ---> Function[x, Let[{...}, expr]][x0]
--
-- This creates a version of Let that shadows variables outside the
-- scope of the Let. For example:
--
-- x = 10;
-- Let[x=9, x] --> 9
--
-- Notice that subsequent Let bindings can refer to previously bound
-- variables:
--
-- Let[{x=9, y=x+1}, y] --> 10
--
letDef :: ListOrSolo SetBind -> Expr -> Expr
letDef (MkListOrSolo bindings) body = Foldable.foldr funApp body bindings
  where
    funApp (MkSetBind x x0) expr = Function :@ Pair (ExprSymbol x) expr :@ Solo x0

---------- Module ----------

moduleDef :: ListOrSolo Symbol -> Expr -> Eval (Maybe Expr)
moduleDef (MkListOrSolo vars) body = go vars
  where
    go varSymbols = do
      modVars <- mapM newModuleSymbol varSymbols
      pure $ do
        let substs = Seq.zipWith MkSubstitution varSymbols (fmap ExprSymbol modVars)
        substSet <- insertSubstitutions substs emptySubstitutionSet
        pure $ applySubstitutionsWithShadowing substSet body

---------- Table ----------

tableDef :: Expr -> TableRange -> Expr
tableDef body range = case range of
  RangeLength n -> List :@ Seq.replicate (fromIntegral n) body
  RangeIndices xVar i j k -> List :@ Seq.unfoldr go i
    where
      go x
        | x > j = Nothing
        | otherwise =
          let
            subst = singletonSubstitutionSet xVar (ExprNumeric x)
            body' = applySubstitutionsWithShadowing subst body
          in
            Just (body', x + k)
  RangeList xVar xs -> List :@ fmap substX xs
    where
      substX x = applySubstitutionsWithShadowing (singletonSubstitutionSet xVar x) body

addScopedBuiltins :: Eval ()
addScopedBuiltins = do
  modifyAttributes "Function" (setHoldType HoldAll)
  addDecl functionDecl

  modifyAttributes "Let" (setHoldType HoldAll)
  def "Let" letDef

  modifyAttributes "Module" (setHoldType HoldAll)
  def "Module" moduleDef

  def "Table" tableDef
