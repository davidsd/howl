{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}

module MicroMath.StdLib where

import Control.Monad     (guard)
import Data.Map.Strict   (Map)
import Data.Map.Strict   qualified as Map
import Data.Sequence     (Seq, pattern (:<|), pattern Empty)
import Data.Sequence     qualified as Seq
import Data.Text         (Text)
import Debug.Trace       qualified as Debug
import MicroMath.Context (Attributes (..), Context (..), ContextM, Rule (..),
                          addDownValue, addPatRule, addUpValue, createContext,
                          emptyAttributes, functionRule, setAttributes)
import MicroMath.Expr    hiding (False, True)
import MicroMath.Expr    qualified as Expr
import MicroMath.Parser  (parseExprText)
import MicroMath.Pat     (Pat, setDelayedFromExpr)
import MicroMath.Symbol  (Symbol)

data Numerics = MkNumerics
  { integers  :: [Integer]
  , rationals :: [Rational]
  , reals     :: [Double]
  } deriving (Eq, Ord, Show)

emptyNumerics :: Numerics
emptyNumerics = MkNumerics [] [] []

addNumeric :: Numeric -> Numerics -> Numerics
addNumeric n nums = case n of
  NInteger i  -> nums { integers  = i:nums.integers }
  NRational r -> nums { rationals = r:nums.rationals }
  NReal r     -> nums { reals     = r:nums.reals }

collapseNumerics :: (forall a . Num a => [a] -> a) -> Numerics -> Expr
collapseNumerics f nums
  | _:_ <- nums.reals = fromReal $ f $
    nums.reals <>
    map realToFrac nums.rationals <>
    map realToFrac nums.integers
  | _:_ <- nums.rationals =
      fromRational $ f $ nums.rationals <> map toRational nums.integers
  | otherwise = fromInteger $ f nums.integers

data PlusArguments = MkPlusArguments
  { plusNums   :: Numerics
  , plusOthers :: Map Expr Numerics
  } deriving (Eq, Ord, Show)

addNumericsMap :: Ord a => a -> Numeric -> Map a Numerics -> Map a Numerics
addNumericsMap k n = Map.alter (Just . addNumeric n . maybe emptyNumerics id) k

emptyPlusArguments :: PlusArguments
emptyPlusArguments = MkPlusArguments emptyNumerics Map.empty

addPlusArgument :: Expr -> PlusArguments -> PlusArguments
addPlusArgument arg plusArgs = case arg of
  ExprNumeric n -> plusArgs { plusNums   = addNumeric n plusArgs.plusNums }
  _             -> plusArgs
    { plusOthers =
      let (term, coeff) = case arg of
            Times :@ (ExprNumeric n :<| t :<| Empty) -> (t, n)
            Times :@ (ExprNumeric n :<| rest)        -> (Times :@ rest, n)
            _                                        -> (arg, NInteger 1)
      in
        addNumericsMap term coeff plusArgs.plusOthers
    }

normalizePlus :: Seq Expr -> Expr
normalizePlus initialArgs =
  case allTerms of
    Empty         -> 0
    (t :<| Empty) -> t
    _             -> Plus :@ (Seq.unstableSort $ flattenWithHead Plus allTerms)
  where
    plusArgs =
      foldr addPlusArgument emptyPlusArguments initialArgs
    numericTerm = collapseNumerics sum plusArgs.plusNums
    otherTerms = Seq.fromList $ do
      (a, cs) <- Map.toList plusArgs.plusOthers
      let coeff = collapseNumerics sum cs
      case (a, coeff) of
        (_, 0)              -> []
        (_, 1)              -> pure a
        (Times :@ terms, _) -> pure $ Times :@ (coeff :<| terms)
        _                   -> pure $ Times :@ (coeff :<| a :<| Empty)
    allTerms =
      (if numericTerm == 0 then id else (numericTerm :<|)) $
      otherTerms

builtinPlusRule :: Rule
builtinPlusRule = functionRule $ \case
  Plus :@ args -> Just $ normalizePlus args
  _ -> Nothing

data TimesArguments = MkTimesArguments
  { timesNums :: Numerics
  , powerArgs :: Map Expr (Seq Expr)
  }

emptyTimesArguments :: TimesArguments
emptyTimesArguments = MkTimesArguments emptyNumerics Map.empty

addTimesArgument :: Expr -> TimesArguments -> TimesArguments
addTimesArgument arg timesArgs = case arg of
  ExprNumeric n -> timesArgs { timesNums = addNumeric n timesArgs.timesNums }
  _             -> timesArgs
    { powerArgs =
      let
        (base, ex) = case arg of
          Power :@ (a :<| b :<| Empty) -> (a, b)
          _                            -> (arg, 1)
      in
        Map.alter (Just . (ex :<|) . maybe Empty id) base timesArgs.powerArgs
    }

normalizeTimes :: Seq Expr -> Expr
normalizeTimes initialArgs =
  case allTerms of
    Empty         -> 1
    (t :<| Empty) -> t
    _             -> Times :@ (Seq.unstableSort $ flattenWithHead Times allTerms)
  where
    timesArgs = foldr addTimesArgument emptyTimesArguments initialArgs
    numericTerm = collapseNumerics product timesArgs.timesNums
    powerTerms = Seq.fromList $ do
      (a, bs) <- Map.toList timesArgs.powerArgs
      guard $ bs /= Seq.singleton 0
      pure $ normalizePower a $ normalizePlus bs
    allTerms =
      (if numericTerm == 1 then id else (numericTerm :<|)) $
      powerTerms

builtinTimesRule :: Rule
builtinTimesRule = functionRule $ \case
  Times :@ args -> Just $ normalizeTimes args
  _ -> Nothing

normalizePower :: Expr -> Expr -> Expr
normalizePower a b = case (a, b) of
  (_, 0)                           -> 1
  (_, 1)                           -> a
  (ExprNumeric na, ExprNumeric nb) -> numericPower na nb
  (_, _)                           -> Expr.binary Power a b

-- TODO: Detect exact rational powers
numericPower :: Numeric -> Numeric -> Expr
numericPower nx ny = case (nx, ny) of
  (NInteger  x, NInteger  y)
    | y >= 0    -> fromInteger $ x^y
    | otherwise -> fromRational $ toRational x^^y
  (NInteger  x, NRational y) -> Expr.binary Power (ExprInteger x) (ExprRational y)
  (NInteger  x, NReal     y) -> fromReal $ realToFrac x ** y
  (NRational x, NInteger  y) -> fromRational $ x^^y
  (NRational x, NRational y) -> Expr.binary Power (ExprRational x) (ExprRational y)
  (NRational x, NReal     y) -> fromReal $ realToFrac x ** y
  (NReal     x, NInteger  y) -> fromReal $ x ** realToFrac y
  (NReal     x, NRational y) -> fromReal $ x ** realToFrac y
  (NReal     x, NReal     y) -> fromReal $ x ** y

builtinPowerRule :: Rule
builtinPowerRule = functionRule $ \case
  Power :@ (x :<| y :<| Empty) -> Just $ normalizePower x y
  _ -> Nothing

sameQRule :: Rule
sameQRule = functionRule $ \case
  SameQ :@ Empty -> Just $ Expr.True
  SameQ :@ (x :<| rest) -> Just $ fromBool $ all (== x) rest
  _ -> Nothing

andRule :: Rule
andRule = functionRule $ \case
  And :@ args -> Just $ case filterBools args of
    Nothing            -> Expr.False
    Just Empty         -> Expr.True
    Just (x :<| Empty) -> x
    Just xs            -> And :@ xs
    where
      filterBools Empty                = Just Empty
      filterBools (Expr.False :<| _)   = Nothing
      filterBools (Expr.True :<| rest) = filterBools rest
      filterBools (x :<| xs)           = fmap (x :<|) (filterBools xs)
  _ -> Nothing

orRule :: Rule
orRule = functionRule $ \case
  Or :@ args -> Just $ case filterBools args of
    Nothing            -> Expr.True
    Just Empty         -> Expr.False
    Just (x :<| Empty) -> x
    Just xs            -> Or :@ xs
    where
      filterBools Empty                 = Just Empty
      filterBools (Expr.True :<| _)     = Nothing
      filterBools (Expr.False :<| rest) = filterBools rest
      filterBools (x :<| xs)            = fmap (x :<|) (filterBools xs)
  _ -> Nothing

downValues :: [(Symbol, Rule)] -> ContextM ()
downValues = mapM_ (uncurry addDownValue)

upValues :: [(Symbol, Rule)] -> ContextM ()
upValues = mapM_ (uncurry addUpValue)

decls :: [Text] -> ContextM ()
decls = mapM_ (uncurry addPatRule . parseDecl)

addStdLib :: ContextM ()
addStdLib = do
  setAttributes "Plus"  $ emptyAttributes { flat = True, orderless = True }
  setAttributes "Times" $ emptyAttributes { flat = True, orderless = True }
  setAttributes "And"   $ emptyAttributes { flat = True }
  setAttributes "Or"    $ emptyAttributes { flat = True }
  downValues
    [ ("Plus",  builtinPlusRule)
    , ("Times", builtinTimesRule)
    , ("Power", builtinPowerRule)
    , ("SameQ", sameQRule)
    , ("And",   andRule)
    , ("Or",    orRule)
    ]
  decls
    [ "UnsameQ[xs___] := Not[SameQ[xs]]"
    , "Not[True]    := False"
    , "Not[False]   := True"
    , "Not[Not[x_]] := x"
    ]

parseDecl :: Text -> (Pat, Expr)
parseDecl declText =
  maybe (error $ "Couldn't parse declaration: " ++ show declText) id $ do
  decl <- parseExprText declText
  setDelayedFromExpr decl

myContext :: Context
myContext = createContext $ do
  addStdLib
  decls
    [ "square[x_] := x*x"
    , "fib[0] := 0"
    , "fib[1] := 1"
    , "fib[n_] := fib[n-1] + fib[n-2]"
    , "buz := False"
    , "main := Not[Or[buz, Not[buz], Not[x]]]"
    ]

