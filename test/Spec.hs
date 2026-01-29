{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Main (main) where

import Data.Sequence           qualified as Seq
import Data.String             (fromString)
import Data.Text               (Text)
import Data.Text               qualified as Text
import MicroMath                (defStdLib, eval, run, runEval)
import MicroMath.Expr         (pattern Part)
import MicroMath.Expr.Internal (Expr (..), pattern ExprBigFloat,
                                pattern ExprDouble, pattern ExprInteger)
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
parseRoundTrip :: Expr -> Either String Expr
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
     parsed === Right normalized

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
      parseRoundTrip "x" `shouldBe` Right "x"

    it "integer" $
      parseRoundTrip (ExprInteger 42) `shouldBe` Right (ExprInteger 42)

    it "simple sum" $
      parseRoundTrip (ExprApp "Plus" (Seq.fromList ["x", "y"]))
        `shouldBe` Right (ExprApp "Plus" (Seq.fromList ["x", "y"]))

    it "simple product" $
      parseRoundTrip (ExprApp "Times" (Seq.fromList ["x", "y"]))
        `shouldBe` Right (ExprApp "Times" (Seq.fromList ["x", "y"]))

    it "power" $
      parseRoundTrip (ExprApp "Power" (Seq.fromList ["x", ExprInteger 2]))
        `shouldBe` Right (ExprApp "Power" (Seq.fromList ["x", ExprInteger 2]))

    it "nested power (right-associative)" $
      parseRoundTrip (ExprApp "Power" (Seq.fromList ["x", ExprApp "Power" (Seq.fromList ["y", "z"])]))
        `shouldBe` Right (ExprApp "Power" (Seq.fromList ["x", ExprApp "Power" (Seq.fromList ["y", "z"])]))

    it "left-nested power (needs parens)" $
      parseRoundTrip (ExprApp "Power" (Seq.fromList [ExprApp "Power" (Seq.fromList ["x", "y"]), "z"]))
        `shouldBe` Right (ExprApp "Power" (Seq.fromList [ExprApp "Power" (Seq.fromList ["x", "y"]), "z"]))

    it "polynomial" $
      let poly = ExprApp "Plus" (Seq.fromList
            [ ExprInteger 1
            , ExprApp "Times" (Seq.fromList [ExprInteger 2, "x"])
            , ExprApp "Times" (Seq.fromList [ExprInteger 3, ExprApp "Power" (Seq.fromList ["x", ExprInteger 2])])
            ])
      in parseRoundTrip poly `shouldBe` Right poly

    it "rule" $
      parseRoundTrip (ExprApp "Rule" (Seq.fromList ["x", "y"]))
        `shouldBe` Right (ExprApp "Rule" (Seq.fromList ["x", "y"]))

    it "nested rule (right-associative)" $
      parseRoundTrip (ExprApp "Rule" (Seq.fromList ["a", ExprApp "Rule" (Seq.fromList ["b", "c"])]))
        `shouldBe` Right (ExprApp "Rule" (Seq.fromList ["a", ExprApp "Rule" (Seq.fromList ["b", "c"])]))

    it "list" $
      parseRoundTrip (ExprApp "List" (Seq.fromList ["x", "y", "z"]))
        `shouldBe` Right (ExprApp "List" (Seq.fromList ["x", "y", "z"]))

    it "function application" $
      parseRoundTrip (ExprApp "f" (Seq.fromList ["x", "y"]))
        `shouldBe` Right (ExprApp "f" (Seq.fromList ["x", "y"]))

  describe "Parser precedence" $ do
    it "parses MapApply with factorial" $
      parseExprText "a@@@b!" `shouldBe`
        Right (ExprApp "Factorial" (Seq.fromList [ExprApp "MapApply" (Seq.fromList ["a", "b"])]))

    it "parses factorial binding tighter than power" $
      parseExprText "a^b!" `shouldBe`
        Right (ExprApp "Power" (Seq.fromList ["a", ExprApp "Factorial" (Seq.fromList ["b"])]))

    it "parses unary minus with factorial" $
      parseExprText "-b!" `shouldBe`
        Right (ExprApp "Times" (Seq.fromList [ExprInteger (-1), ExprApp "Factorial" (Seq.fromList ["b"])]))

    it "parses prefix apply lower precedence than bracket apply" $
      parseExprText "a@b[c][d]" `shouldBe`
        Right (ExprApp "a" (Seq.fromList [ExprApp (ExprApp "b" (Seq.fromList ["c"])) (Seq.fromList ["d"])]))

    it "parses prefix apply as right associative (wl syntax)" $
      parseExprText "a@b@c" `shouldBe` parseExprText "a[b[c]]"

    it "parses mixed prefix apply and bracket apply (wl syntax)" $
      parseExprText "a@b[c]@d" `shouldBe` parseExprText "a[b[c][d]]"

    it "parses multiple bracket applies with prefix apply (wl syntax)" $
      parseExprText "a@b[c][d][e]" `shouldBe` parseExprText "a[b[c][d][e]]"

  describe "CompoundExpression parsing" $ do
    it "parses trailing semicolon as Null at end of input" $
      parseExprText "a;" `shouldBe` parseExprText "a;Null"

    it "parses trailing semicolon before comma as Null" $
      parseExprText "f[a;, b]" `shouldBe` parseExprText "f[a;Null, b]"

  describe "Part parsing" $ do
    it "parses double brackets into Part" $
      parseExprText "a[[b,c]]"
        `shouldBe` Right (ExprApp Part (Seq.fromList ["a", "b", "c"]))

    it "parses chained part access" $
      parseExprText "a[[b]][[c]]"
        `shouldBe` Right (ExprApp Part (Seq.fromList [ExprApp Part (Seq.fromList ["a", "b"]), "c"]))

  describe "BigFloat parsing" $ do
    it "parses explicit precision with backtick" $ do
      let parsed = parseExprText "1.25`40"
      case parsed of
        Right (ExprBigFloat bf) -> Rounded.precision bf `shouldBe` decimalDigitsToBits 40
        _ -> expectationFailure "expected BigFloat"

    it "parses long decimal as BigFloat with precision from digits" $ do
      let parsed = parseExprText "3.1415926535897932384626"
      case parsed of
        Right (ExprBigFloat bf) -> Rounded.precision bf `shouldBe` decimalDigitsToBits 22
        _ -> expectationFailure "expected BigFloat"

    it "keeps short decimal as Double" $
      parseExprText "2.718281828459045" `shouldBe` Right (ExprDouble 2.718281828459045)

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

      it "uses optional pattern when argument is present" $ do
        result <- eval' "Foo[a,b,c] /. {Foo[Optional[x_,def],b,c] :> f[x]}"
        pPrint result `shouldBe` "f[a]"

      it "uses optional pattern default when argument is omitted" $ do
        result <- eval' "Foo[b,c] /. {Foo[Optional[x_,def],b,c] :> f[x]}"
        pPrint result `shouldBe` "f[def]"

      it "handles multiple optional arguments" $ do
        result <- eval' "Foo[] /. {Foo[Optional[x_,dx], Optional[y_,dy]] :> {x,y}}"
        pPrint result `shouldBe` "{dx, dy}"

      it "prefers provided optional arguments over defaults" $ do
        result <- eval' "Foo[a] /. {Foo[Optional[x_,dx], Optional[y_,dy]] :> {x,y}}"
        pPrint result `shouldBe` "{a, dy}"

      it "respects head constraints inside optional patterns" $ do
        result <- eval' "Foo[1] /. {Foo[Optional[x_Integer, dx]] :> x}"
        pPrint result `shouldBe` "1"

      it "uses Default[head] for Optional[a_] when present" $ do
        result <- eval' "Foo[1] /. {Foo[Optional[a_]] :> a}"
        pPrint result `shouldBe` "1"

      it "uses Default[head] for Optional[a_] when omitted" $ do
        result <- eval' "Foo[] /. {Foo[Optional[a_]] :> a}"
        pPrint result `shouldBe` "Default[Foo]"

      it "uses Default[Times] when Optional is omitted under Times" $ do
        result <- eval' "Hold[Times[]] /. {Hold[Times[Optional[a_]]] :> a}"
        pPrint result `shouldBe` "1"

      it "uses Default[Plus] when Optional is omitted under Plus" $ do
        result <- eval' "Hold[Plus[]] /. {Hold[Plus[Optional[a_]]] :> a}"
        pPrint result `shouldBe` "0"

      it "prefers provided args under Times and Plus" $ do
        resultTimes <- eval' "Hold[Times[2]] /. {Hold[Times[Optional[a_]]] :> a}"
        pPrint resultTimes `shouldBe` "2"
        resultPlus <- eval' "Hold[Plus[3]] /. {Hold[Plus[Optional[a_]]] :> a}"
        pPrint resultPlus `shouldBe` "3"

      it "uses defaults for And/Or when Optional is omitted" $ do
        resultAnd <- eval' "Hold[And[]] /. {Hold[And[Optional[a_]]] :> a}"
        pPrint resultAnd `shouldBe` "True"
        resultOr <- eval' "Hold[Or[]] /. {Hold[Or[Optional[a_]]] :> a}"
        pPrint resultOr `shouldBe` "False"

      it "handles multiple arguments with Optional defaults" $ do
        result <- eval' "Foo[a,b] /. {Foo[Optional[x_,dx], Optional[y_,dy], Optional[z_,dz]] :> {x,y,z}}"
        pPrint result `shouldBe` "{a, b, dz}"

      it "handles multiple arguments under Plus with Optional defaults" $ do
        result <- eval' "Hold[Plus[1, 2]] /. {Hold[Plus[Optional[x_], Optional[y_], Optional[z_]]] :> {x,y,z}}"
        pPrint result `shouldBe` "{1, 2, 0}"

      it "binds missing alt-pattern vars to Sequence[] (left branch)" $ do
        result <- eval' "Foo[1] /. {Foo[x_Integer | y_String] :> {x, Baz[y]}}"
        pPrint result `shouldBe` "{1, Baz[]}"

      it "binds missing alt-pattern vars to Sequence[] (right branch)" $ do
        result <- eval' "Foo[\"s\"] /. {Foo[x_Integer | y_String] :> {Baz[x], y}}"
        pPrint result `shouldBe` "{Baz[], \"s\"}"

    describe "Part" $ do
      it "extracts a list element" $ do
        result <- eval' "{1, 2, 3}[[2]]"
        pPrint result `shouldBe` "2"

      it "extracts nested parts from nested lists" $ do
        result <- eval' "{{a, b}, {c, d}}[[2, 1]]"
        pPrint result `shouldBe` "c"

      it "supports negative indices from the end" $ do
        result <- eval' "{1, 2, 3}[[ -1 ]]"
        pPrint result `shouldBe` "3"

      it "supports nested parts on symbols" $ do
        result <- eval' "f[g[h]][[1]][[1]]"
        pPrint result `shouldBe` "h"

      it "handles index 0 with list of indices (Mathematica behavior)" $ do
        result <- eval' "f[g][[{0, 0, 0}]]"
        pPrint result `shouldBe` "f[f, f, f]"

      it "handles index 0 on atoms (Mathematica behavior)" $ do
        result <- eval' "1[[{0, 0, 0}]]"
        pPrint result `shouldBe` "Integer[Integer, Integer, Integer]"

      it "does not simplify when any index is non-integer" $ do
        result <- eval' "f[g[h]][[1, foo]]"
        pPrint result `shouldBe` "Part[f[g[h]], 1, foo]"

      it "keeps nested part expression when out of range" $ do
        result <- eval' "{1, 2}[[3, 1]]"
        pPrint result `shouldBe` "Part[{1, 2}, 3, 1]"

    describe "Head" $ do
      it "returns the head of an expression" $ do
        result <- eval' "Head[f[x]]"
        pPrint result `shouldBe` "f"

      it "returns the head of an integer" $ do
        result <- eval' "Head[1]"
        pPrint result `shouldBe` "Integer"

      it "returns the head of a list" $ do
        result <- eval' "Head[{1, 2}]"
        pPrint result `shouldBe` "List"

    describe "Lists" $ do
      it "creates lists" $ do
        result <- eval' "{1, 2, 3}"
        pPrint result `shouldBe` "{1, 2, 3}"

      it "computes Length of a list" $ do
        result <- eval' "Length[{1, 2, 3}]"
        pPrint result `shouldBe` "3"

      it "takes a prefix of a list" $ do
        result <- eval' "Take[{1, 2, 3}, 2]"
        pPrint result `shouldBe` "{1, 2}"

      it "drops a prefix of a list" $ do
        result <- eval' "Drop[{1, 2, 3}, 2]"
        pPrint result `shouldBe` "{3}"

      it "maps over lists" $ do
        result <- eval' "Map[f, {1, 2, 3}]"
        pPrint result `shouldBe` "{f[1], f[2], f[3]}"

      it "applies function to list" $ do
        result <- eval' "Apply[Plus, {1, 2, 3}]"
        pPrint result `shouldBe` "6"

      it "counts matches in a list" $ do
        result <- eval' "Count[{a, b, a, c, a}, a]"
        pPrint result `shouldBe` "3"

      it "counts pattern matches in a list" $ do
        result <- eval' "Count[{1, 2, 3, 4}, _Integer]"
        pPrint result `shouldBe` "4"

      it "returns positions of matching elements" $ do
        result <- eval' "Position[{a, b, a, c, a}, a]"
        pPrint result `shouldBe` "{{1}, {3}, {5}}"

      it "returns positions for pattern matches" $ do
        result <- eval' "Position[{1, 2, 3, 4}, _Integer]"
        pPrint result `shouldBe` "{{1}, {2}, {3}, {4}}"

    describe "Flatten" $ do
      it "flattens nested lists" $ do
        result <- eval' "Flatten[{a, {b, c}, {d, {e}}}]"
        pPrint result `shouldBe` "{a, b, c, d, e}"

      it "flattens nested applications with the same head" $ do
        result <- eval' "Flatten[f[a, f[b, c], f[d, f[e]]]]"
        pPrint result `shouldBe` "f[a, b, c, d, e]"

      it "does not flatten different heads" $ do
        result <- eval' "Flatten[f[a, g[b, c]]]"
        pPrint result `shouldBe` "f[a, g[b, c]]"

    describe "Union" $ do
      it "computes the union of lists" $ do
        result <- eval' "Union[{3, 2, 1, 2}, {2, 4}]"
        pPrint result `shouldBe` "{1, 2, 3, 4}"

    describe "Intersection" $ do
      it "computes the intersection of lists" $ do
        result <- eval' "Intersection[{3, 2, 1, 2}, {2, 4}]"
        pPrint result `shouldBe` "{2}"

    describe "StdLib extras" $ do
      it "reverses lists" $ do
        result <- eval' "Reverse[{1, 2, 3}]"
        pPrint result `shouldBe` "{3, 2, 1}"

      it "maps at a position" $ do
        result <- eval' "MapAt[f, g[a, b, c], 2]"
        pPrint result `shouldBe` "g[a, f[b], c]"

      it "builds a simple table with an index" $ do
        result <- eval' "Table[i, {i, 3}]"
        pPrint result `shouldBe` "{1, 2, 3}"

      it "builds a table over a list of values" $ do
        result <- eval' "Table[i^2, {i, {1, 3, 5}}]"
        pPrint result `shouldBe` "{1, 9, 25}"

      it "evaluates If with True/False branches" $ do
        resultTrue <- eval' "If[True, 1, 2]"
        pPrint resultTrue `shouldBe` "1"
        resultFalse <- eval' "If[False, 1, 2]"
        pPrint resultFalse `shouldBe` "2"

      it "replaces repeatedly until fixed point" $ do
        result <- eval' "x //. {x -> y, y -> z}"
        pPrint result `shouldBe` "z"

      it "checks SameQ" $ do
        resultSame <- eval' "SameQ[1, 1, 1]"
        pPrint resultSame `shouldBe` "True"

      it "checks OrderedQ" $ do
        resultOrdered <- eval' "OrderedQ[{1, 2, 3}]"
        pPrint resultOrdered `shouldBe` "True"

      it "checks MemberQ" $ do
        resultMember <- eval' "MemberQ[{1, 2, 3}, 2]"
        pPrint resultMember `shouldBe` "True"

      it "checks Not" $ do
        resultNot <- eval' "Not[Not[True]]"
        pPrint resultNot `shouldBe` "True"

      it "evaluates Let bindings" $ do
        result <- eval' "Let[{x = 2, y = x + 3}, y]"
        pPrint result `shouldBe` "5"

    describe "Expand" $ do
      it "expands simple product" $ do
        result <- eval' "Expand[(x + 1)(x + 2)]"
        pPrint result `shouldBe` "2 + x^2 + 3 x"

      it "expands power of binomial" $ do
        result <- eval' "Expand[(x + 1)^2]"
        pPrint result `shouldBe` "1 + x^2 + 2 x"

    describe "Symbolic/Numeric consistency" $ do
      prop "symbolic eval then substitute == substitute then eval" prop_symbolicNumericConsistency
