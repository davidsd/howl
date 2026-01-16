{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module MicroMath.Expr.Internal where

import Data.Foldable    qualified as Foldable
import Data.List        (intercalate)
import Data.Ratio       (denominator, numerator)
import Data.Sequence    (Seq, pattern (:<|), pattern Empty)
import Data.Sequence    qualified as Seq
import Data.String      (IsString (..))
import Data.Text        (Text)
import MicroMath.PPrint (PPrint (..))
import MicroMath.Symbol (Symbol)

-- | NB: The ordering of constructors is chosen so that the derived
-- Ord instance gives the correct ordering of expressions.
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

-- | NB: The ordering of constructors is chosen so that the derived
-- Ord instance gives the correct ordering of expressions.
data Expr
  = ExprLit !Literal
  | ExprSymbol {-# UNPACK #-} !Symbol
  | ExprApp !Expr !(Seq Expr)
  deriving (Eq, Ord)

pattern (:@) :: Expr -> Seq Expr -> Expr
pattern h :@ cs = ExprApp h cs
infixl 9 :@

-- | A class for types that can potentially be matched to an
-- expression.
class FromExpr a where
  fromExpr :: Expr -> Maybe a

instance FromExpr Expr where
  fromExpr = Just

-- | A class for types that can be converted to an Expr
class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr Expr where
  toExpr = id

-- | A general pattern synonym that matches anything with a FromExpr
-- instance.
pattern ExprView :: FromExpr a => a -> Expr
pattern ExprView a <- (fromExpr -> Just a)

instance FromExpr Symbol where
  fromExpr = \case { ExprSymbol sym -> Just sym; _ -> Nothing }
instance ToExpr Symbol where
  toExpr = ExprSymbol

pattern ExprInteger :: Integer -> Expr
pattern ExprInteger n = ExprLit (LitInteger n)

instance FromExpr Integer where
  fromExpr = \case { ExprInteger x -> Just x; _ -> Nothing }
instance ToExpr Integer where
  toExpr = ExprInteger

pattern ExprRational :: Rational -> Expr
pattern ExprRational q = ExprLit (LitRational q)

-- | Convert a Rational into an Expr, representing it as an
-- ExprInteger if it is an integer.
exprFromRational :: Rational -> Expr
exprFromRational r
  | denominator r == 1 = ExprInteger (numerator r)
  | otherwise          = ExprRational r

instance FromExpr Rational where
  fromExpr = \case { ExprRational x -> Just x; _ -> Nothing }
instance ToExpr Rational where
  toExpr = exprFromRational

pattern ExprReal :: Double -> Expr
pattern ExprReal x = ExprLit (LitReal x)

instance FromExpr Double where
  fromExpr = \case { ExprReal x -> Just x; _ -> Nothing }
instance ToExpr Double where
  toExpr = ExprReal

pattern ExprString :: Text -> Expr
pattern ExprString s = ExprLit (LitString s)

instance FromExpr Text where
  fromExpr = \case { ExprString x -> Just x; _ -> Nothing }
instance ToExpr Text where
  toExpr = ExprString
  
data Numeric
  = NInteger  !Integer
  | NRational !Rational
  | NReal     !Double
  deriving (Eq)

instance Ord Numeric where
  compare (NInteger x)  (NInteger y)  = compare x y
  compare (NRational x) (NInteger y)  = compare x (fromIntegral y)
  compare (NReal x)     (NInteger y)  = compare x (realToFrac y)
  compare (NInteger x)  (NRational y) = compare (fromIntegral x) y
  compare (NRational x) (NRational y) = compare x y
  compare (NReal x)     (NRational y) = compare x (realToFrac y)
  compare (NInteger x)  (NReal y)     = compare (realToFrac x) y
  compare (NRational x) (NReal y)     = compare (realToFrac x) y
  compare (NReal x)     (NReal y)     = compare x y

numericView :: Expr -> Maybe Numeric
numericView (ExprLit (LitInteger n))  = Just (NInteger n)
numericView (ExprLit (LitRational q)) = Just (NRational q)
numericView (ExprLit (LitReal x))     = Just (NReal x)
numericView _                         = Nothing

instance FromExpr Numeric where
  fromExpr = numericView

instance ToExpr Numeric where
  toExpr (NInteger n)  = toExpr n
  toExpr (NRational q) = toExpr q
  toExpr (NReal x)     = toExpr x

pattern ExprNumeric :: Numeric -> Expr
pattern ExprNumeric n <- (numericView -> Just n)
  where
    ExprNumeric = toExpr

{-# COMPLETE ExprSymbol, ExprLit, (:@) #-}
{-# COMPLETE ExprInteger, ExprRational, ExprReal, ExprString, ExprSymbol, (:@) #-}

instance IsString Expr where
  fromString = ExprSymbol . fromString

unary :: Expr -> Expr -> Expr
unary e x = ExprApp e (Seq.singleton x)

binary :: Expr -> Expr -> Expr -> Expr
binary e x y = ExprApp e (Seq.fromList [x,y])

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

-- | Apply the OneIdentity rule with the given default element.
applyOneId :: Expr -> Expr -> Seq Expr -> Expr
applyOneId def h exprs = case Seq.filter (/= def) exprs of
  Seq.Empty   -> def
  x :<| Empty -> x
  xs          -> h :@ xs

rootSymbol :: Expr -> Maybe Symbol
rootSymbol = \case
  ExprSymbol s -> Just s
  ExprApp h _  -> rootSymbol h
  _            -> Nothing

