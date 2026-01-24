{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module MicroMath.Pat
  ( Pat(..)
  , SeqType(..)
  , mapNames
  , addNames
  , patRootSymbol
  , patFromExpr
  ) where

import Data.Foldable      qualified as Foldable
import Data.List          (intercalate)
import Data.Sequence      (Seq, pattern (:<|), pattern Empty)
import Data.String        (IsString (..))
import MicroMath.Expr     (Expr (..), Literal, pattern Alternatives,
                           pattern Blank, pattern BlankNullSequence,
                           pattern BlankSequence, pattern Pattern, pattern Test)
import MicroMath.Expr.PPrint ()
import MicroMath.PPrint   (PPrint (..))
import MicroMath.Symbol   (Symbol)

{- | TODO:

Composite Patterns
- p..(Repeated), p...(RepeatedNull)
- Except
- Longest
- Shortest
- OptionsPattern, PatternSequence, Verbatim, HoldPattern
- OrderlessPatternSequence
- KeyValuePattern

Restrictions on Patterns

- PatternTest (?) -- Can be implemented in terms of Condition

Pattern Defaults
_:e (Optional) pattern that defaults to e if omitted
_. (Optional) pattern with predefined default
Default - predefined default arguments for a function

-}

-- | Pat: A pattern. Each constructor takes a [Symbol] argument, which
-- is a list of variables, each of which should be bound to the result
-- of the pattern match. We need a list because we can have something
-- like x:(y:(z:_)). If the list is empty, the pattern is unnamed.
data Pat
  = PatSymbol    ![Symbol] {-# UNPACK #-} !Symbol
  | PatLit       ![Symbol] !Literal
    -- | The second argument is an optional head constraint.
  | PatVar       ![Symbol] !(Maybe Symbol)
  | PatSeqVar    ![Symbol] !SeqType
  | PatApp       ![Symbol] !Pat !(Seq Pat)
  | PatAlt       ![Symbol] !Pat !Pat
  | PatCondition ![Symbol] !Pat !Expr
  deriving (Eq, Ord, Show)

data SeqType = ZeroOrMore | OneOrMore
  deriving (Eq, Ord, Show)

instance IsString Pat where
  fromString = PatSymbol [] . fromString

mapNames :: ([Symbol] -> [Symbol]) -> Pat -> Pat
mapNames f = \case
  PatSymbol    names l      -> PatSymbol    (f names) l
  PatLit       names l      -> PatLit       (f names) l
  PatVar       names h      -> PatVar       (f names) h
  PatSeqVar    names ty     -> PatSeqVar    (f names) ty
  PatApp       names h cs   -> PatApp       (f names) h cs
  PatAlt       names p1 p2  -> PatAlt       (f names) p1 p2
  PatCondition names p expr -> PatCondition (f names) p expr

addNames :: [Symbol] -> Pat -> Pat
addNames xs = mapNames (xs++)

addName :: Symbol -> Pat -> Pat
addName xs = mapNames (xs:)

pPrintNamed :: [Symbol] -> String -> String
pPrintNamed [] s = s
pPrintNamed (x:xs) s = concat
  [ pPrint x
  , ":("
  , pPrintNamed xs s
  , ")"
  ]

instance PPrint Pat where
  pPrint (PatSymbol names l) = pPrintNamed names (pPrint l)
  pPrint (PatLit    names l) = pPrintNamed names (pPrint l)
  pPrint (PatVar names h) =
    let blankStr = "_" <> maybe "" pPrint h
    in case names of
      []  -> blankStr
      [x] -> pPrint x <> blankStr
      _   -> pPrintNamed names blankStr
  pPrint (PatSeqVar names seqTy) =
    let blankStr = case seqTy of
          ZeroOrMore -> "___"
          OneOrMore  -> "__"
    in case names of
      []  -> blankStr
      [x] -> pPrint x <> blankStr
      _   -> pPrintNamed names blankStr
  pPrint (PatApp names f args) = pPrintNamed names $
    concat
    [ pPrint f
    , "["
    , intercalate ", " (map pPrint (Foldable.toList args))
    , "]"
    ]
  pPrint (PatAlt names p1 p2) = pPrintNamed names $
    pPrint p1 ++ "|" ++ pPrint p2
  pPrint (PatCondition names p t) = pPrintNamed names $
    pPrint p ++ "/;" ++ pPrint t

-- | The patRootSymbol is the repeated head. If the patRootSymbol is a Symbol,
-- return it, otherwise return nothing. This function is needed for
-- automatically deducing which symbol to associate a rule to.
patRootSymbol :: Pat -> Maybe Symbol
patRootSymbol = \case
  PatSymbol _ s      -> Just s
  PatApp _ h _       -> patRootSymbol h
  PatCondition _ p _ -> patRootSymbol p
  _                  -> Nothing

patFromExpr :: Expr -> Pat
patFromExpr expr = case expr of
  ExprApp Pattern (ExprSymbol x :<| expr' :<| Empty) ->
    addName x $ patFromExpr expr'
  ExprApp Blank Empty                    -> PatVar [] Nothing
  ExprApp Blank (ExprSymbol h :<| Empty) -> PatVar [] (Just h)
  ExprApp BlankSequence Empty            -> PatSeqVar [] OneOrMore
  ExprApp BlankNullSequence Empty        -> PatSeqVar [] ZeroOrMore
  ExprApp Alternatives pExprs@(_ :<| _) ->
    foldr1 (PatAlt []) $ fmap patFromExpr pExprs
  ExprApp Test (pExpr :<| cond :<| Empty) -> PatCondition [] (patFromExpr pExpr) cond
  ExprLit lit    -> PatLit [] lit
  ExprSymbol sym -> PatSymbol [] sym
  ExprApp h cs   -> PatApp [] (patFromExpr h) (fmap patFromExpr cs)
