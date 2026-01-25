{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module MicroMath.Expr.Internal
  ( Literal(..)
  , Expr(..)
  , pattern (:@)
  , pattern ExprView
  , pattern ExprNumeric
  , pattern ExprInteger
  , pattern ExprRational
  , pattern ExprReal
  , pattern ExprString
  , FromExpr(..)
  , ToExpr(..)
  , unary
  , binary
  , mapSymbols
  , flattenWithHead
  , applyOneId
  , rootSymbol
  ) where

import Data.Foldable          qualified as Foldable
import Data.List              (intercalate)
import Data.Ratio             (denominator, numerator)
import Data.Sequence          (Seq, pattern (:<|), pattern Empty)
import Data.Sequence          qualified as Seq
import Data.String            (IsString (..))
import Data.Text              (Text)
import MicroMath.Expr.Numeric (Numeric (..))
import MicroMath.PPrint       (PPrint (..))
import MicroMath.Symbol       (Symbol)

-- | NB: The ordering of constructors is chosen so that the derived
-- Ord instance gives the correct ordering of expressions.
data Literal
  = LitNumeric !Numeric
  | LitString !Text
  deriving (Eq, Ord, Show)

instance PPrint Literal where
  pPrint (LitNumeric x) = show x
  pPrint (LitString s)  = show s

-- | NB: The ordering of constructors is chosen so that the derived
-- Ord instance gives the correct ordering of expressions.
data Expr
  = ExprLit !Literal
  -- | ExprSymbol {-# UNPACK #-} !Symbol
  | ExprSymbol !Symbol
  | ExprApp !Expr !(Seq Expr)
  deriving (Eq, Ord)

instance IsString Expr where
  fromString = ExprSymbol . fromString

pattern (:@) :: Expr -> Seq Expr -> Expr
pattern h :@ cs = ExprApp h cs
infixl 9 :@

{-# COMPLETE ExprSymbol, ExprLit, (:@) #-}

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

pattern ExprNumeric :: Numeric -> Expr
pattern ExprNumeric x = ExprLit (LitNumeric x)

instance FromExpr Numeric where
  fromExpr = \case { ExprNumeric n -> Just n; _ -> Nothing}
instance ToExpr Numeric where
  toExpr = ExprLit . LitNumeric

pattern ExprInteger :: Integer -> Expr
pattern ExprInteger n = ExprNumeric (NInteger n)

instance FromExpr Integer where
  fromExpr = \case { ExprInteger x -> Just x; _ -> Nothing }
instance ToExpr Integer where
  toExpr = ExprInteger

pattern ExprRational :: Rational -> Expr
pattern ExprRational q = ExprNumeric (NRational q)

instance FromExpr Rational where
  fromExpr = \case { ExprRational x -> Just x; _ -> Nothing }

-- | Convert a Rational into an Expr, representing it as an
-- ExprInteger if it is an integer.
instance ToExpr Rational where
  toExpr r
    | denominator r == 1 = ExprInteger (numerator r)
    | otherwise          = ExprRational r

pattern ExprReal :: Double -> Expr
pattern ExprReal x = ExprNumeric (NReal x)

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

-- Warning: This could cause overflow
instance FromExpr Int where
  fromExpr = \case { ExprInteger x -> Just (fromIntegral x); _ -> Nothing }
instance ToExpr Int where
  toExpr = ExprInteger . fromIntegral

unary :: Expr -> Expr -> Expr
unary e x = ExprApp e (Seq.singleton x)

binary :: Expr -> Expr -> Expr -> Expr
binary e x y = ExprApp e (Seq.fromList [x,y])

instance Show Expr where
  show (ExprSymbol s) = pPrint s
  show (ExprLit l) = pPrint l
  show (ExprApp f args) =
    mconcat
    [ show f
    , "["
    , intercalate ", " (map show (Foldable.toList args))
    , "]"
    ]

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
