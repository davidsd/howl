{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

module Howl.Builtins.Data
  ( addDataBuiltins
  ) where

import Data.Foldable       qualified as Foldable
import Data.Sequence       (Seq, pattern (:<|), pattern (:|>), pattern Empty)
import Data.Sequence       qualified as Seq
import Data.Set            qualified as Set
import Howl.Eval           (MatchingEq (..), solveMatchMaybe)
import Howl.Eval.Context   (Eval, HoldType (..), compilePat,
                            modifyAttributes, setHoldType)
import Howl.Expr           (Expr (..), Numeric (..), pattern (:@),
                            pattern ExprBigFloat, pattern ExprDouble,
                            pattern ExprInteger,
                            pattern ExprRational, pattern ExprString,
                            pattern ExprSymbol, pattern List)
import Howl.Expr           qualified as Expr
import Howl.Builtins.Algebra (normalizePlus)
import Howl.Builtins.Types   (AList (..))
import Howl.ToBuiltin      (Variadic (..), def)
import Howl.Util           (pattern Pair, pattern Solo)

---------- Map ----------

mapDef :: Expr -> Expr -> Maybe Expr
mapDef f = \case
  h :@ xs -> Just $ h :@ fmap (Expr.unary f) xs
  _       -> Nothing

mapApplyDef :: Expr -> Expr -> Maybe Expr
mapApplyDef f = \case
  h :@ xs -> Just $ h :@ fmap replaceHead xs
  _       -> Nothing
  where
    replaceHead = \case
      _ :@ ys -> f :@ ys
      expr    -> expr

---------- MapAt ----------

mapAtDef :: Expr -> Expr -> Int -> Maybe Expr
mapAtDef fExpr (ExprApp h cs) n
  | n == 0
  = Just $ ExprApp (Expr.unary fExpr h) cs
  | n > 0 && n <= Seq.length cs
  = Just $ ExprApp h $ Seq.adjust' (Expr.unary fExpr) (n - 1) cs
  | n < 0 && -n <= Seq.length cs
  = Just $ ExprApp h $ Seq.adjust' (Expr.unary fExpr) (Seq.length cs + n) cs
  | otherwise = Nothing
mapAtDef _ _ _ = Nothing

---------- Length ----------

lengthDef :: Expr -> Int
lengthDef = \case
  ExprApp _ xs -> Seq.length xs
  _            -> 0

---------- Part ----------

partDef :: Seq Expr -> Maybe Expr
partDef Empty = Nothing
partDef (expr :<| indices) = go expr indices
  where
    go e Empty = Just e
    go e (List :@ inds :<| inds') = do
      eHead <- exprHead e
      ExprApp eHead
        <$> mapM (\i -> go e (i :<| inds')) inds
    go (ExprApp h' cs) (ExprInteger i :<| inds')
      | i == 0
      = go h' inds'
      | Just c <- Seq.lookup (fromInteger i - 1) cs
      = go c inds'
      | i < 0
      , let idx = Seq.length cs + fromInteger i
      , Just c <- Seq.lookup idx cs
      = go c inds'
      | otherwise = Nothing
    go _ (Solo (ExprInteger 0)) = exprHead expr
    go _ _ = Nothing

exprHead :: Expr -> Maybe Expr
exprHead (ExprApp h _)    = Just h
exprHead (ExprSymbol _)   = Just "Symbol"
exprHead (ExprInteger _)  = Just "Integer"
exprHead (ExprRational _) = Just "Rational"
exprHead (ExprDouble _)   = Just "Double"
exprHead (ExprBigFloat _) = Just "BigFloat"
exprHead (ExprString _)   = Just "String"
exprHead _                = Nothing

---------- Take/Drop ----------

takeDef :: AList Expr -> Int -> Maybe (AList Expr)
takeDef (MkList xs) n
  | n <= Seq.length xs = Just $ MkList (Seq.take n xs)
  | otherwise          = Nothing

dropDef :: AList Expr -> Int -> Maybe (AList Expr)
dropDef (MkList xs) n
  | n <= Seq.length xs = Just $ MkList (Seq.drop n xs)
  | otherwise          = Nothing

---------- Range ----------

rangeDef :: Numeric -> Numeric -> Numeric -> AList Numeric
rangeDef i j di = MkList $ Seq.fromList [i, i + di .. j]

---------- Accumulate ----------

accumulateDef :: AList Expr -> AList Expr
accumulateDef (MkList xs) = MkList $ case xs of
  Empty      -> Empty
  x :<| rest -> snd $ Foldable.foldl' step (x, Solo x) rest
  where
    step (acc, out) y =
      let acc' = normalizePlus (Pair acc y)
      in (acc', out :|> acc')

firstDef :: Expr -> Maybe Expr
firstDef = \case
  ExprApp _ (x :<| _) -> Just x
  _                   -> Nothing

restDef :: Expr -> Maybe Expr
restDef = \case
  ExprApp h (_ :<| xs) -> Just (ExprApp h xs)
  _                    -> Nothing

---------- Reverse ----------

reverseDef :: Expr -> Maybe Expr
reverseDef = \case
  ExprApp h xs -> Just $ ExprApp h (Seq.reverse xs)
  _            -> Nothing

---------- Union ----------

unionDef :: Seq (AList Expr) -> AList Expr
unionDef = MkList . Seq.fromList . Set.toList . Set.unions . fmap toSet
  where
    toSet (MkList xs) = Set.fromList (Foldable.toList xs)

---------- Intersection ----------

intersectionDef :: Seq (AList Expr) -> AList Expr
intersectionDef = MkList . Seq.fromList . Set.toList . intersectAll . fmap toSet
  where
    toSet (MkList xs) = Set.fromList (Foldable.toList xs)
    intersectAll = \case
      Empty    -> Set.empty
      s :<| ss -> Foldable.foldl' Set.intersection s ss

---------- Ordering ----------

orderingDef :: AList Expr -> AList Int
orderingDef (MkList xs) = MkList $
  fmap (succ . snd) $
  Seq.unstableSortOn fst $
  Seq.mapWithIndex (\i x -> (x, i)) xs

orderingNDef :: AList Expr -> Int -> AList Int
orderingNDef xs n = MkList . Seq.take n . (.unList) . orderingDef $ xs

---------- Sort ----------

sortDef :: AList Expr -> AList Expr
sortDef (MkList xs) = MkList $ Seq.unstableSort xs

---------- Count and Position ----------

countDef :: Expr -> Expr -> Eval Int
countDef expr patExpr = case expr of
  ExprApp _ xs -> do
    pat <- compilePat patExpr
    let
      go e i = do
        maybeMatch <- solveMatchMaybe (SingleEq pat e)
        pure $ case maybeMatch of
          Just _  -> i + 1
          Nothing -> i
    Foldable.foldrM go 0 xs
  _ -> pure 0

positionDef :: Expr -> Expr -> Eval (AList (AList Int))
positionDef expr patExpr = do
  pat <- compilePat patExpr
  case expr of
    ExprApp h xs -> do
      let
        go :: Seq Int -> Int -> Seq Expr -> Eval (Seq Int)
        go acc _ Empty = pure acc
        go acc i (y :<| ys) = do
          maybeMatch <- solveMatchMaybe (SingleEq pat y)
          case maybeMatch of
            Just _  -> go (acc :|> i) (i + 1) ys
            Nothing -> go acc (i + 1) ys
      positions <- go Empty 0 (h :<| xs)
      pure $ MkList $ fmap (MkList . Solo) positions
    _ -> do
      maybeMatch <- solveMatchMaybe (SingleEq pat expr)
      pure $ case maybeMatch of
        Just _  -> MkList (Solo (MkList Empty))
        Nothing -> MkList Empty

---------- Flatten ----------

flattenDef :: Expr -> Maybe Expr
flattenDef = \case
  ExprApp h cs -> Just $ ExprApp h (Expr.flattenWithHead h cs)
  _            -> Nothing

addDataBuiltins :: Eval ()
addDataBuiltins = do
  def "Map" mapDef
  def "MapApply" mapApplyDef
  def "MapAt" mapAtDef
  def "Head" exprHead
  def "Part" (MkVariadic partDef)
  def "Length" lengthDef

  def "Range" rangeDef
  def "Range" (\i j -> rangeDef i j 1)
  def "Range" (\i -> rangeDef 1 i 1)

  def "Take" takeDef
  def "Drop" dropDef
  def "Accumulate" accumulateDef
  def "First" firstDef
  def "Rest" restDef
  def "Reverse" reverseDef
  def "Union" (MkVariadic unionDef)
  def "Intersection" (MkVariadic intersectionDef)
  def "Ordering" orderingDef
  def "Ordering" orderingNDef
  def "Sort" sortDef
  def "Flatten" flattenDef
  modifyAttributes "Count" (setHoldType HoldRest)
  def "Count" countDef
  modifyAttributes "Position" (setHoldType HoldRest)
  def "Position" positionDef
