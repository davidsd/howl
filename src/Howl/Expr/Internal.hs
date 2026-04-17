{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Core expression datatypes, conversions, and low-level helpers.
module Howl.Expr.Internal
  ( Literal(..)
  , Expr(..)
  , pattern (:@)
  , pattern ExprView
  , pattern ExprNumeric
  , pattern ExprInteger
  , pattern ExprRational
  , pattern ExprDouble
  , pattern ExprBigFloat
  , pattern ExprString
  , FromExpr(..)
  , ToExpr(..)
  , fullForm
  , unary
  , binary
  , mapSymbols
  , flattenWithHead
  , applyOneId
  , rootSymbol
  ) where

import Data.Foldable     qualified as Foldable
import Data.List         (intercalate)
import Data.Ratio        (denominator, numerator)
import Data.Sequence     (Seq, pattern (:<|), pattern Empty)
import Data.Sequence     qualified as Seq
import Data.String       (IsString (..))
import Data.Text         (Text)
import Howl.Expr.Numeric (BigFloat, Numeric (..))
import Howl.PPrint       (PPrint (..))
import Howl.Symbol       (Symbol)

-- | A literal value.
data Literal
  = -- | A numeric literal.
    LitNumeric !Numeric
  | -- | A string literal.
    LitString !Text
  deriving (Eq, Ord, Show)

instance PPrint Literal where
  pPrint (LitNumeric x) = pPrint x
  pPrint (LitString s)  = show s

-- | A Wolfram Language expression.
data Expr
  = -- | A literal value such as a number or string.
    ExprLit !Literal
  | -- | A symbol expression such as @x@, @Plus@, or @List@.
    ExprSymbol !Symbol
  | -- | An application of a head expression to a sequence of arguments.
    --
    -- For example:
    --
    -- - @Plus[1, x]@
    -- - @f[a, b, c]@
    -- - @g[x][y, z]@
    ExprApp !Expr !(Seq Expr)
  deriving (Eq, Ord, Show)

instance IsString Expr where
  fromString = ExprSymbol . fromString

-- | Render an expression in full form.
fullForm :: Expr -> String
fullForm = \case
  ExprSymbol s   -> pPrint s
  ExprLit l      -> pPrint l
  ExprApp f args ->
    mconcat
    [ fullForm f
    , "["
    , intercalate ", " (map fullForm (Foldable.toList args))
    , "]"
    ]

-- | An infix version of 'ExprApp'.
--
-- @f :\@ args@ is equivalent to @ExprApp f args@.
pattern (:@) :: Expr -> Seq Expr -> Expr
pattern h :@ cs = ExprApp h cs
infixl 9 :@

{-# COMPLETE ExprSymbol, ExprLit, (:@) #-}

-- | A class for types that can potentially be matched to expressions.
class FromExpr a where
  fromExpr :: Expr -> Maybe a

instance FromExpr Expr where
  fromExpr = Just

-- | A class for types that can be converted to expressions.
class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr Expr where
  toExpr = id

-- | A general pattern synonym that matches any value with a
-- 'FromExpr' instance.
pattern ExprView :: FromExpr a => a -> Expr
pattern ExprView a <- (fromExpr -> Just a)

instance FromExpr Symbol where
  fromExpr = \case { ExprSymbol sym -> Just sym; _ -> Nothing }
instance ToExpr Symbol where
  toExpr = ExprSymbol

-- | A pattern synonym for numeric expressions.
pattern ExprNumeric :: Numeric -> Expr
pattern ExprNumeric x = ExprLit (LitNumeric x)

instance FromExpr Numeric where
  fromExpr = \case { ExprNumeric n -> Just n; _ -> Nothing}
instance ToExpr Numeric where
  toExpr = ExprLit . LitNumeric

-- | A pattern synonym for integer expressions.
pattern ExprInteger :: Integer -> Expr
pattern ExprInteger n = ExprNumeric (NInteger n)

instance FromExpr Integer where
  fromExpr = \case { ExprInteger x -> Just x; _ -> Nothing }
instance ToExpr Integer where
  toExpr = ExprInteger

-- | A pattern synonym for rational expressions.
pattern ExprRational :: Rational -> Expr
pattern ExprRational q = ExprNumeric (NRational q)

instance FromExpr Rational where
  fromExpr = \case
    ExprInteger n  -> Just $ fromIntegral n
    ExprRational x -> Just x
    _              -> Nothing

-- | Convert a Rational into an Expr, representing it as an
-- ExprInteger if it is an integer.
instance ToExpr Rational where
  toExpr r
    | denominator r == 1 = ExprInteger (numerator r)
    | otherwise          = ExprRational r

-- | A pattern synonym for double-precision floating-point expressions.
pattern ExprDouble :: Double -> Expr
pattern ExprDouble x = ExprNumeric (NDouble x)

instance FromExpr Double where
  fromExpr = \case { ExprDouble x -> Just x; _ -> Nothing }
instance ToExpr Double where
  toExpr = ExprDouble

-- | A pattern synonym for arbitrary-precision floating-point expressions.
pattern ExprBigFloat :: BigFloat -> Expr
pattern ExprBigFloat x = ExprNumeric (NBigFloat x)

instance FromExpr BigFloat where
  fromExpr = \case { ExprBigFloat x -> Just x; _ -> Nothing }
instance ToExpr BigFloat where
  toExpr = ExprBigFloat

-- | A pattern synonym for string expressions.
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

-- | Apply a head to a single argument.
unary :: Expr -> Expr -> Expr
unary e x = ExprApp e (Seq.singleton x)

-- | Apply a head to two arguments.
binary :: Expr -> Expr -> Expr -> Expr
binary e x y = ExprApp e (Seq.fromList [x,y])

-- | Map a function over the symbols present in an expression.
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

-- | Get the outermost symbol of an expression, if there is one.
rootSymbol :: Expr -> Maybe Symbol
rootSymbol = \case
  ExprSymbol s -> Just s
  ExprApp h _  -> rootSymbol h
  _            -> Nothing
