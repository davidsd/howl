{-# LANGUAGE OverloadedStrings #-}

module MicroMath.Pat
  ( Pat(..)
  , SeqType(..)
  , mapNames
  , addNames
  ) where

import Data.List        (intercalate)
import Data.String      (IsString (..))
import MicroMath.Expr   (Expr (..), Literal (..), Symbol)
import MicroMath.PPrint (PPrint (..))

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
  = PatAtom [Symbol] Literal
    -- | The second argument is an optional head constraint.
  | PatVar [Symbol] (Maybe Symbol)
  | PatSeqVar [Symbol] SeqType
  | PatApp [Symbol] Pat [Pat]
  | PatAlt [Symbol] Pat Pat
  | PatCondition [Symbol] Pat Expr
  deriving (Eq, Ord, Show)

data SeqType = ZeroOrMore | OneOrMore
  deriving (Eq, Ord, Show)

instance IsString Pat where
  fromString = PatAtom [] . fromString

mapNames :: ([Symbol] -> [Symbol]) -> Pat -> Pat
mapNames f pat = case pat of
  PatAtom      names l      -> PatAtom      (f names) l
  PatVar       names h      -> PatVar       (f names) h
  PatSeqVar    names ty     -> PatSeqVar    (f names) ty
  PatApp       names h cs   -> PatApp       (f names) h cs
  PatAlt       names p1 p2  -> PatAlt       (f names) p1 p2
  PatCondition names p expr -> PatCondition (f names) p expr

addNames :: [Symbol] -> Pat -> Pat
addNames xs = mapNames (xs++)

pPrintNamed :: [Symbol] -> String -> String
pPrintNamed [] s = s
pPrintNamed (x:xs) s = concat
  [ pPrint x
  , ":("
  , pPrintNamed xs s
  , ")"
  ]

instance PPrint Pat where
  pPrint (PatAtom names l) = pPrintNamed names (pPrint l)
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
    , intercalate ", " (map pPrint args)
    , "]"
    ]
  pPrint (PatAlt names p1 p2) = pPrintNamed names $
    pPrint p1 ++ "|" ++ pPrint p2
  pPrint (PatCondition names p t) = pPrintNamed names $
    pPrint p ++ "/;" ++ pPrint t
