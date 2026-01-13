{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TemplateHaskell   #-}

module MicroMath.Expr where

import Prelude qualified as Prelude
import Data.Foldable     qualified as Foldable
import Data.List         (intercalate)
import Data.Ratio        (denominator, numerator)
import Data.Sequence     (Seq, pattern Empty, pattern (:<|))
import Data.Sequence     qualified as Seq
import Data.String       (IsString (..))
import Data.Text         (Text)
import MicroMath.Expr.TH (declareBuiltins)
import MicroMath.PPrint  (PPrint (..))
import MicroMath.Symbol  (Symbol(..), mkSymbol)
import Prelude           hiding (False, True)

data Literal
  = LitInteger !Integer
  | LitRational !Rational
  | LitReal !Double -- TODO: Multiprecision
  | LitString !Text
  deriving (Eq, Ord, Show)

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

data Expr
  = ExprSymbol {-# UNPACK #-} !Symbol
  | ExprLit !Literal
  | ExprApp !Expr !(Seq Expr)
  deriving (Eq, Ord)

pattern (:@) :: Expr -> Seq Expr -> Expr
pattern h :@ cs = ExprApp h cs
infixl 9 :@

data Numeric
  = NInteger  !Integer
  | NRational !Rational
  | NReal     !Double

numericView :: Expr -> Maybe Numeric
numericView (ExprLit (LitInteger n))  = Just (NInteger n)
numericView (ExprLit (LitRational q)) = Just (NRational q)
numericView (ExprLit (LitReal x))     = Just (NReal x)
numericView _                          = Nothing

pattern ExprNumeric :: Numeric -> Expr
pattern ExprNumeric n <- (numericView -> Just n)
  where
    ExprNumeric (NInteger n)  = ExprInteger n
    ExprNumeric (NRational q) = ExprRational q
    ExprNumeric (NReal x)     = ExprReal x
  
pattern ExprInteger :: Integer -> Expr
pattern ExprInteger n = ExprLit (LitInteger n)

pattern ExprRational :: Rational -> Expr
pattern ExprRational q = ExprLit (LitRational q)

pattern ExprReal :: Double -> Expr
pattern ExprReal x = ExprLit (LitReal x)

pattern ExprString :: Text -> Expr
pattern ExprString s = ExprLit (LitString s)
  
{-# COMPLETE ExprSymbol, ExprLit, (:@) #-}
{-# COMPLETE ExprInteger, ExprRational, ExprReal, ExprString, ExprSymbol, (:@) #-}

instance IsString Expr where
  fromString = ExprSymbol . fromString

unary :: Expr -> Expr -> Expr
unary e x = ExprApp e (Seq.singleton x)

binary :: Expr -> Expr -> Expr -> Expr
binary e x y = ExprApp e (Seq.fromList [x,y])

mkSymbolAtom :: Text -> Expr
mkSymbolAtom = ExprSymbol . mkSymbol

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
   , "Function"
   , "List"
   , "Apply"
   , "Map"
   , "Let"
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
   , "OrderedQ"
   , "Rule"
   , "RuleDelayed"
   , "ReplaceAll"
   , "ReplaceRepeated"
   , "Set"
   , "SetDelayed"
   , "UpSet"
   , "UpSetDelayed"
   , "Not"
   ])
   
instance Num Expr where
  (+) = binary Plus
  (*) = binary Times
  negate x = binary Times (fromInteger (-1)) x
  abs = unary Abs
  signum = unary Sign
  fromInteger = ExprLit . LitInteger

instance Fractional Expr where
  recip x = binary Power x (fromInteger (-1))
  fromRational r = ExprLit $
    if denominator r == 1
    then LitInteger (numerator r)
    else LitRational r

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

fromReal :: Double -> Expr
fromReal = ExprReal

fromBool :: Bool -> Expr
fromBool Prelude.True = True
fromBool Prelude.False = False

boolView :: Expr -> Maybe Bool
boolView True = Just Prelude.True
boolView False = Just Prelude.False
boolView _                          = Nothing

pattern ExprBool :: Bool -> Expr
pattern ExprBool b <- (boolView -> Just b)
  where
    ExprBool b = fromBool b

instance PPrint Expr where
  pPrint (ExprSymbol s) = pPrint s
  pPrint (ExprLit l) = pPrint l
  pPrint (ExprApp f args) =
    mconcat
    [ pPrint f
    , "["
    , intercalate ", " (map pPrint (Foldable.toList args))
    , "]"
    ]

instance Show Expr where
  show = pPrint

-- | Map a function over the symbols present in an expression
mapSymbols :: (Symbol -> Expr) -> Expr -> Expr
mapSymbols f expr = case expr of
  ExprSymbol s -> f s
  ExprLit _    -> expr
  ExprApp h cs -> ExprApp (mapSymbols f h) (fmap (mapSymbols f) cs)

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
