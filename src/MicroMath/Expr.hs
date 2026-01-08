{-# LANGUAGE OverloadedStrings #-}

module MicroMath.Expr
  ( Symbol(..)
  , Literal(..)
  , Expr(..)
  , unary
  , binary
  , mapSymbols
  , flattenWithHead
  , flattenSequences
  ) where

import Data.List        (intercalate)
import Data.Ratio       (denominator, numerator)
import Data.String      (IsString (..))
import Data.Text        (Text)
import Data.Text        qualified as Text
import MicroMath.PPrint (PPrint (..))

newtype Symbol = MkSymbol Text
  deriving (Eq, Ord, Show)

instance PPrint Symbol where
  pPrint (MkSymbol s) = Text.unpack s

instance IsString Symbol where
  fromString = MkSymbol . fromString

data Literal
  = LitInteger Integer
  | LitRational Rational
  | LitString Text
  | LitSymbol Symbol
  deriving (Eq, Ord, Show)

instance IsString Literal where
  fromString = LitSymbol . fromString

showRational :: Rational -> String
showRational r =
  show (numerator r) ++
  if denominator r == 1
  then ""
  else "/" ++ show (denominator r)

instance PPrint Literal where
  pPrint (LitInteger i)  = show i
  pPrint (LitRational r) = showRational r
  pPrint (LitString s)   = show s
  pPrint (LitSymbol s)   = pPrint s

data Expr
  = ExprAtom Literal
  | ExprApp Expr [Expr]
  deriving (Eq, Ord, Show)

instance IsString Expr where
  fromString = ExprAtom . fromString

unary :: Expr -> Expr -> Expr
unary e x = ExprApp e [x]

binary :: Expr -> Expr -> Expr -> Expr
binary e x y = ExprApp e [x,y]

instance Num Expr where
  (+) = binary "Plus"
  (*) = binary "Times"
  negate x = (-1)*x
  abs = unary "Abs"
  signum = unary "Signum"
  fromInteger = ExprAtom . LitInteger

instance Fractional Expr where
  recip x = ExprApp "Power" [x,-1]
  fromRational = ExprAtom . LitRational

instance Floating Expr where
  pi = "Pi"
  exp = unary "Exp"
  log = unary "Log"
  sqrt x = x ** fromRational (1/2)
  logBase = binary "Log"
  (**) = binary "Power"
  sin = unary "Sin"
  cos = unary "Cos"
  tan = unary "Tan"
  asin = unary "ArcSin"
  acos = unary "ArcCos"
  atan = unary "ArcTan"
  sinh = unary "Sinh"
  cosh = unary "Cosh"
  tanh = unary "Tanh"
  asinh = unary "ArcSinh"
  acosh = unary "ArcCosh"
  atanh = unary "ArcTanh"

instance PPrint Expr where
  pPrint (ExprAtom l) = pPrint l
  pPrint (ExprApp f args) =
    mconcat
    [ pPrint f
    , "["
    , intercalate ", " (map pPrint args)
    , "]"
    ]

-- | Map a function over the symbols present in an expression
mapSymbols :: (Symbol -> Expr) -> Expr -> Expr
mapSymbols f expr = case expr of
  ExprAtom (LitSymbol s) -> f s
  ExprAtom _             -> expr
  ExprApp h cs           -> ExprApp (mapSymbols f h) (map (mapSymbols f) cs)

-- | Flatten applications of h in the given list of expressions,
-- working recursively until a different head is encountered. Example:
--
-- flattenWithHead "A" [A[x], A[A[y],B[A[z]]]]
-- ---> [x,y,B[A[z]]]
flattenWithHead :: Expr -> [Expr] -> [Expr]
flattenWithHead h exprs = do
  expr <- exprs
  case expr of
    ExprApp h' args | h' == h -> flattenWithHead h args
    _                         -> pure expr

-- | Flatten any occurrences of Sequence[...] in the given list of
-- expressions. Note that this works with nested Sequence as well,
-- e.g.  flattenSequences [Sequence[Sequence[a]],b] -> [a,b]
flattenSequences :: [Expr] -> [Expr]
flattenSequences = flattenWithHead "Sequence"
