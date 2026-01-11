{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TemplateHaskell   #-}

module MicroMath.Expr where

import Data.Foldable     qualified as Foldable
import Data.List         (intercalate)
import Data.Ratio        (denominator, numerator)
import Data.Scientific   (Scientific)
import Data.Sequence     (Seq, pattern Empty, pattern (:<|))
import Data.Sequence     qualified as Seq
import Data.String       (IsString (..))
import Data.Text         (Text)
import MicroMath.Expr.TH (declareBuiltins)
import MicroMath.PPrint  (PPrint (..))
import MicroMath.Symbol  (Symbol, mkSymbol)
import Prelude           hiding (False, True)

data Literal
  = LitInteger !Integer
  | LitRational !Rational
  | LitReal !Scientific
  | LitString !Text
  | LitSymbol !Symbol
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
  pPrint (LitReal x)     = show x
  pPrint (LitString s)   = show s
  pPrint (LitSymbol s)   = pPrint s

data Expr
  = ExprAtom !Literal
  | ExprApp !Expr !(Seq Expr)
  deriving (Eq, Ord, Show)

pattern (:@) :: Expr -> Seq Expr -> Expr
pattern h :@ cs = ExprApp h cs
infixl 9 :@

pattern ExprInteger :: Integer -> Expr
pattern ExprInteger n = ExprAtom (LitInteger n)

pattern ExprRational :: Rational -> Expr
pattern ExprRational q = ExprAtom (LitRational q)

pattern ExprReal :: Scientific -> Expr
pattern ExprReal x = ExprAtom (LitReal x)

pattern ExprString :: Text -> Expr
pattern ExprString s = ExprAtom (LitString s)

pattern ExprSymbol :: Symbol -> Expr
pattern ExprSymbol s = ExprAtom (LitSymbol s)
  
{-# COMPLETE ExprAtom, (:@) #-}
{-# COMPLETE ExprInteger, ExprRational, ExprReal, ExprString, ExprSymbol, (:@) #-}

instance IsString Expr where
  fromString = ExprAtom . fromString

unary :: Expr -> Expr -> Expr
unary e x = ExprApp e (Seq.singleton x)

binary :: Expr -> Expr -> Expr -> Expr
binary e x y = ExprApp e (Seq.fromList [x,y])

mkSymbolAtom :: Text -> Expr
mkSymbolAtom = ExprAtom . LitSymbol . mkSymbol

-- | declareBuiltins creates bidirectional pattern synonyms Sequence,
-- List, etc. The main advantage of these over using the IsString
-- instance for Expr and writing "Sequence", "List", etc, is that for
-- the pattern synonyms we call mkSymbol at top level precisely once,
-- whereas each time our code uses the IsString instance, it needs to
-- use 'mkSymbol' which hashes the given text and looks it up in the
-- symbol table.
--
$(declareBuiltins ''Expr 'mkSymbolAtom
   [ "Sequence"
   , "List"
   , "Map"
   , "Plus"
   , "Subtract"
   , "Times"
   , "Divide"
   , "Abs"
   , "Sign"
   , "Power"
   , "Blank"
   , "BlankSequence"
   , "BlankNullSequence"
   , "Slot"
   , "SlotSequence"
   , "Pattern"
   , "Alternatives"
   , "Optional"
   , "Test"
   , "Association"
   , "Pi"
   , "Exp"
   , "Log"
   , "Sin"
   , "Cos"
   , "Tan"
   , "ArcSin"
   , "ArcCos"
   , "ArcTan"
   , "Sinh"
   , "Cosh"
   , "Tanh"
   , "ArcSinh"
   , "ArcCosh"
   , "ArcTanh"
   , "And"
   , "Or"
   , "True"
   , "False"
   , "Less"
   , "Greater"
   , "LessEqual"
   , "GreaterEqual"
   , "Equal"
   , "Unequal"
   , "SameQ"
   , "UnsameQ"
   , "Rule"
   , "RuleDelayed"
   , "ReplaceAll"
   , "ReplaceRepeated"
   , "Set"
   , "SetDelayed"
   , "UpSet"
   , "UpSetDelayed"
   ])
   
instance Num Expr where
  (+) = binary Plus
  (*) = binary Times
  negate x = binary Times (fromInteger (-1)) x
  abs = unary Abs
  signum = unary Sign
  fromInteger = ExprAtom . LitInteger

instance Fractional Expr where
  recip x = binary Power x (fromInteger (-1))
  fromRational = ExprAtom . LitRational

instance Floating Expr where
  pi = Pi
  exp = unary Exp
  log = unary Log
  sqrt x = x ** fromRational (1/2)
  logBase = binary Log
  (**) = binary Power
  sin = unary Sin
  cos = unary Cos
  tan = unary Tan
  asin = unary ArcSin
  acos = unary ArcCos
  atan = unary ArcTan
  sinh = unary Sinh
  cosh = unary Cosh
  tanh = unary Tanh
  asinh = unary ArcSinh
  acosh = unary ArcCosh
  atanh = unary ArcTanh

instance PPrint Expr where
  pPrint (ExprAtom l) = pPrint l
  pPrint (ExprApp f args) =
    mconcat
    [ pPrint f
    , "["
    , intercalate ", " (map pPrint (Foldable.toList args))
    , "]"
    ]

-- | Map a function over the symbols present in an expression
mapSymbols :: (Symbol -> Expr) -> Expr -> Expr
mapSymbols f expr = case expr of
  ExprAtom (LitSymbol s) -> f s
  ExprAtom _             -> expr
  ExprApp h cs           -> ExprApp (mapSymbols f h) (fmap (mapSymbols f) cs)

-- | Flatten applications of h in the given list of expressions,
-- working recursively until a different head is encountered. Example:
--
-- flattenWithHead "A" [A[x], A[A[y],B[A[z]]]]
-- ---> [x,y,B[A[z]]]
flattenWithHead :: Expr -> Seq Expr -> Seq Expr
flattenWithHead h exprs = do
  expr <- exprs
  case expr of
    ExprApp h' args | h' == h -> flattenWithHead h args
    _                         -> pure expr

-- | Flatten any occurrences of Sequence[...] in the given list of
-- expressions. Note that this works with nested Sequence as well,
-- e.g.  flattenSequences [Sequence[Sequence[a]],b] -> [a,b]
flattenSequences :: Seq Expr -> Seq Expr
flattenSequences = flattenWithHead Sequence

-- | Apply the OneIdentity rule with the given default element.
applyOneId :: Expr -> Expr -> Seq Expr -> Expr
applyOneId def h exprs = case Seq.filter (/= def) exprs of
  Seq.Empty   -> def
  x :<| Empty -> x
  xs          -> h :@ xs
