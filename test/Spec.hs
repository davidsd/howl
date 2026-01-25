{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Main (main) where

import Data.Sequence           qualified as Seq
import Data.String             (fromString)
import Data.Text               (Text)
import Data.Text               qualified as Text
import MicroMath                (defStdLib, eval, run, runEval)
import MicroMath.Expr.Internal (Expr (..), pattern ExprBigFloat,
                                pattern ExprInteger, pattern ExprReal)
import MicroMath.Expr.PPrint   ()
import MicroMath.Parser        (normalizeParsedExpr, parseExprText)
import MicroMath.PPrint        (PPrint (..))
import Numeric.Rounded.Simple qualified as Rounded
import Test.Hspec
import Test.Hspec.QuickCheck   (prop)
import Test.QuickCheck

-- | Fixed set of symbol names to avoid GC issues with interned symbols
-- These symbols are kept alive for the duration of the test
knownSymbols :: [Expr]
knownSymbols = map fromString
  ["x", "y", "z", "a", "b", "c", "f", "g", "h", "n", "m", "k"]
{-# NOINLINE knownSymbols #-}

genSymbol :: Gen Expr
genSymbol = elements knownSymbols

newtype ArbExpr = ArbExpr { unArbExpr :: Expr }
  deriving (Show)

instance Arbitrary ArbExpr where
  arbitrary = ArbExpr <$> sized genExpr
  shrink (ArbExpr expr) = ArbExpr <$> shrinkExpr expr

genExpr :: Int -> Gen Expr
genExpr 0 = oneof [genSymbol, genInteger]
genExpr n = frequency
  [ (3, genSymbol)
  , (3, genInteger)
  , (2, genPlus n)
  , (2, genTimes n)
  , (2, genPower n)
  , (1, genRule n)
  , (1, genList n)
  ]

genInteger :: Gen Expr
genInteger = ExprInteger <$> choose (0, 1000)

genPlus :: Int -> Gen Expr
genPlus n = do
  len <- choose (2, 4)
  args <- vectorOf len (genExpr (n `div` 2))
  pure $ ExprApp "Plus" (Seq.fromList args)

genTimes :: Int -> Gen Expr
genTimes n = do
  len <- choose (2, 4)
  args <- vectorOf len (genExpr (n `div` 2))
  pure $ ExprApp "Times" (Seq.fromList args)

genPower :: Int -> Gen Expr
genPower n = do
  base <- genExpr (n `div` 2)
  expo <- genExpr (n `div` 2)
  pure $ ExprApp "Power" (Seq.fromList [base, expo])

genRule :: Int -> Gen Expr
genRule n = do
  lhs <- genExpr (n `div` 2)
  rhs <- genExpr (n `div` 2)
  pure $ ExprApp "Rule" (Seq.fromList [lhs, rhs])

genList :: Int -> Gen Expr
genList n = do
  len <- choose (0, 3)
  args <- vectorOf len (genExpr (n `div` 2))
  pure $ ExprApp "List" (Seq.fromList args)

shrinkExpr :: Expr -> [Expr]
shrinkExpr (ExprApp h args) =
  filter isValidExpr (toList args) ++
  [ExprApp h (Seq.fromList args')
    | args' <- shrinkList shrinkExpr (toList args)
    , isValidNaryApp h args'
  ]
  where
    toList = foldr (:) []
shrinkExpr _ = []

-- | Check if an expression is valid (not a bare n-ary operator head)
isValidExpr :: Expr -> Bool
isValidExpr (ExprSymbol s) = s `notElem` ["Plus", "Times", "And", "Or", "Alternatives"]
isValidExpr _ = True

-- | Check if an n-ary application has enough arguments
isValidNaryApp :: Expr -> [Expr] -> Bool
isValidNaryApp h args
  | h `elem` ["Plus", "Times", "And", "Or", "Alternatives"] = length args >= 2
  | h `elem` ["Power", "Rule", "RuleDelayed", "Set", "SetDelayed"] = length args == 2
  | h == "List" = True
  | otherwise = True

-- | Parse an expression from its pretty-printed form
parseRoundTrip :: Expr -> Maybe Expr
parseRoundTrip e = parseExprText (Text.pack (pPrint e))

-- | Property: pretty printing then parsing should give back the normalized form
prop_roundTrip :: ArbExpr -> Property
prop_roundTrip (ArbExpr expr) =
  let printed = pPrint expr
      parsed = parseRoundTrip expr
      normalized = normalizeParsedExpr expr
  in counterexample ("Pretty printed: " ++ printed) $
     counterexample ("Parsed back: " ++ show parsed) $
     counterexample ("Normalized: " ++ show normalized) $
     parsed === Just normalized

-- | Generate a symbolic expression using only x, y, z as variables
genSymbolicExpr :: Int -> Gen Expr
genSymbolicExpr 0 = oneof [elements ["x", "y", "z"], ExprInteger <$> choose (1, 5)]
genSymbolicExpr n = frequency
  [ (2, elements ["x", "y", "z"])
  , (2, ExprInteger <$> choose (1, 5))
  , (1, do  -- Plus
      len <- choose (2, 3)
      args <- vectorOf len (genSymbolicExpr (n `div` 2))
      pure $ ExprApp "Plus" (Seq.fromList args))
  , (1, do  -- Times
      len <- choose (2, 3)
      args <- vectorOf len (genSymbolicExpr (n `div` 2))
      pure $ ExprApp "Times" (Seq.fromList args))
  , (1, do  -- Power with small positive exponent
      base <- genSymbolicExpr (n `div` 2)
      expo <- ExprInteger <$> choose (1, 3)
      pure $ ExprApp "Power" (Seq.fromList [base, expo]))
  ]

-- | Generate substitution values for x, y, z
genSubstitution :: Gen (Integer, Integer, Integer)
genSubstitution = (,,) <$> choose (1, 10) <*> choose (1, 10) <*> choose (1, 10)

-- | Build a ReplaceAll expression: expr /. {x -> xVal, y -> yVal, z -> zVal}
mkReplaceAll :: Expr -> Integer -> Integer -> Integer -> Expr
mkReplaceAll expr xVal yVal zVal =
  ExprApp "ReplaceAll" $ Seq.fromList
    [ expr
    , ExprApp "List" $ Seq.fromList
        [ ExprApp "Rule" (Seq.fromList ["x", ExprInteger xVal])
        , ExprApp "Rule" (Seq.fromList ["y", ExprInteger yVal])
        , ExprApp "Rule" (Seq.fromList ["z", ExprInteger zVal])
        ]
    ]

-- | Property: symbolic evaluation then substitution equals substitution then evaluation
prop_symbolicNumericConsistency :: Property
prop_symbolicNumericConsistency = forAll ((,) <$> sized genSymbolicExpr <*> genSubstitution) $
  \(expr, (xVal, yVal, zVal)) -> ioProperty $ do
    -- Method 1: Evaluate symbolically first, then substitute
    symbolicThenSubst <- runEval $ do
      defStdLib
      symbolic <- eval expr
      eval $ mkReplaceAll symbolic xVal yVal zVal

    -- Method 2: Substitute first (into original), then evaluate
    substThenEval <- runEval $ do
      defStdLib
      eval $ mkReplaceAll expr xVal yVal zVal

    pure $ counterexample ("Expression: " ++ pPrint expr) $
           counterexample ("Substitution: x=" ++ show xVal ++ ", y=" ++ show yVal ++ ", z=" ++ show zVal) $
           counterexample ("Symbolic then subst: " ++ pPrint symbolicThenSubst) $
           counterexample ("Subst then eval: " ++ pPrint substThenEval) $
           symbolicThenSubst === substThenEval

main :: IO ()
main = hspec $ do
  let decimalDigitsToBits :: Int -> Int
      decimalDigitsToBits digits =
        ceiling (fromIntegral digits * logBase 2 (10 :: Double))

  describe "PPrint/Parse round-trip" $ do
    prop "parse . pPrint == id" prop_roundTrip

  describe "Specific round-trip cases" $ do
    it "simple symbol" $
      parseRoundTrip "x" `shouldBe` Just "x"

    it "integer" $
      parseRoundTrip (ExprInteger 42) `shouldBe` Just (ExprInteger 42)

    it "simple sum" $
      parseRoundTrip (ExprApp "Plus" (Seq.fromList ["x", "y"]))
        `shouldBe` Just (ExprApp "Plus" (Seq.fromList ["x", "y"]))

    it "simple product" $
      parseRoundTrip (ExprApp "Times" (Seq.fromList ["x", "y"]))
        `shouldBe` Just (ExprApp "Times" (Seq.fromList ["x", "y"]))

    it "power" $
      parseRoundTrip (ExprApp "Power" (Seq.fromList ["x", ExprInteger 2]))
        `shouldBe` Just (ExprApp "Power" (Seq.fromList ["x", ExprInteger 2]))

    it "nested power (right-associative)" $
      parseRoundTrip (ExprApp "Power" (Seq.fromList ["x", ExprApp "Power" (Seq.fromList ["y", "z"])]))
        `shouldBe` Just (ExprApp "Power" (Seq.fromList ["x", ExprApp "Power" (Seq.fromList ["y", "z"])]))

    it "left-nested power (needs parens)" $
      parseRoundTrip (ExprApp "Power" (Seq.fromList [ExprApp "Power" (Seq.fromList ["x", "y"]), "z"]))
        `shouldBe` Just (ExprApp "Power" (Seq.fromList [ExprApp "Power" (Seq.fromList ["x", "y"]), "z"]))

    it "polynomial" $
      let poly = ExprApp "Plus" (Seq.fromList
            [ ExprInteger 1
            , ExprApp "Times" (Seq.fromList [ExprInteger 2, "x"])
            , ExprApp "Times" (Seq.fromList [ExprInteger 3, ExprApp "Power" (Seq.fromList ["x", ExprInteger 2])])
            ])
      in parseRoundTrip poly `shouldBe` Just poly

    it "rule" $
      parseRoundTrip (ExprApp "Rule" (Seq.fromList ["x", "y"]))
        `shouldBe` Just (ExprApp "Rule" (Seq.fromList ["x", "y"]))

    it "nested rule (right-associative)" $
      parseRoundTrip (ExprApp "Rule" (Seq.fromList ["a", ExprApp "Rule" (Seq.fromList ["b", "c"])]))
        `shouldBe` Just (ExprApp "Rule" (Seq.fromList ["a", ExprApp "Rule" (Seq.fromList ["b", "c"])]))

    it "list" $
      parseRoundTrip (ExprApp "List" (Seq.fromList ["x", "y", "z"]))
        `shouldBe` Just (ExprApp "List" (Seq.fromList ["x", "y", "z"]))

    it "function application" $
      parseRoundTrip (ExprApp "f" (Seq.fromList ["x", "y"]))
        `shouldBe` Just (ExprApp "f" (Seq.fromList ["x", "y"]))

  describe "BigFloat parsing" $ do
    it "parses explicit precision with backtick" $ do
      let parsed = parseExprText "1.25`40"
      case parsed of
        Just (ExprBigFloat bf) -> Rounded.precision bf `shouldBe` decimalDigitsToBits 40
        _ -> expectationFailure "expected BigFloat"

    it "parses long decimal as BigFloat with precision from digits" $ do
      let parsed = parseExprText "3.1415926535897932384626"
      case parsed of
        Just (ExprBigFloat bf) -> Rounded.precision bf `shouldBe` decimalDigitsToBits 22
        _ -> expectationFailure "expected BigFloat"

    it "keeps short decimal as Real" $
      parseExprText "2.718281828459045" `shouldBe` Just (ExprReal 2.718281828459045)

  describe "Evaluator" $ do
    let eval' :: Text -> IO Expr
        eval' input = runEval (defStdLib >> run input)

    describe "Arithmetic" $ do
      it "adds integers" $
        eval' "2 + 3" `shouldReturn` ExprInteger 5

      it "multiplies integers" $
        eval' "4 * 5" `shouldReturn` ExprInteger 20

      it "computes powers" $
        eval' "2^10" `shouldReturn` ExprInteger 1024

      it "handles negative numbers" $
        eval' "5 - 8" `shouldReturn` ExprInteger (-3)

      it "respects order of operations" $
        eval' "2 + 3 * 4" `shouldReturn` ExprInteger 14

      it "handles parentheses" $
        eval' "(2 + 3) * 4" `shouldReturn` ExprInteger 20

    describe "Symbolic expressions" $ do
      it "keeps symbolic sums" $ do
        result <- eval' "x + y"
        pPrint result `shouldBe` "x + y"

      it "keeps symbolic products" $ do
        result <- eval' "x y"
        pPrint result `shouldBe` "x y"

      it "simplifies x + 0" $ do
        result <- eval' "x + 0"
        pPrint result `shouldBe` "x"

      it "simplifies x * 1" $ do
        result <- eval' "x * 1"
        pPrint result `shouldBe` "x"

      it "simplifies x * 0" $ do
        result <- eval' "x * 0"
        pPrint result `shouldBe` "0"

    describe "Pattern matching" $ do
      it "applies simple rule with ReplaceAll" $ do
        result <- eval' "x /. x -> 5"
        pPrint result `shouldBe` "5"

      it "applies rule inside expression" $ do
        result <- eval' "(x + y) /. x -> 1"
        pPrint result `shouldBe` "1 + y"

      it "applies multiple rules" $ do
        result <- eval' "(x + y) /. {x -> 1, y -> 2}"
        pPrint result `shouldBe` "3"

      it "matches patterns with blanks" $ do
        result <- eval' "f[3] /. f[n_] -> n^2"
        pPrint result `shouldBe` "9"

      it "matches single-arg Flat+Orderless head as whole expression" $ do
        result <- eval' "a + b + c /. Plus[x_] -> f[x]"
        pPrint result `shouldBe` "f[a + b + c]"

    describe "Lists" $ do
      it "creates lists" $ do
        result <- eval' "{1, 2, 3}"
        pPrint result `shouldBe` "{1, 2, 3}"

      it "maps over lists" $ do
        result <- eval' "Map[f, {1, 2, 3}]"
        pPrint result `shouldBe` "{f[1], f[2], f[3]}"

      it "applies function to list" $ do
        result <- eval' "Apply[Plus, {1, 2, 3}]"
        pPrint result `shouldBe` "6"

    describe "Expand" $ do
      it "expands simple product" $ do
        result <- eval' "Expand[(x + 1)(x + 2)]"
        pPrint result `shouldBe` "2 + x^2 + 3 x"

      it "expands power of binomial" $ do
        result <- eval' "Expand[(x + 1)^2]"
        pPrint result `shouldBe` "1 + x^2 + 2 x"

    describe "Symbolic/Numeric consistency" $ do
      prop "symbolic eval then substitute == substitute then eval" prop_symbolicNumericConsistency
