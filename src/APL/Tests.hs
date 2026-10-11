module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, VName)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import APL.Eval (eval, runEval)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , sized
  , withMaxSuccess
  , frequency
  , elements
  , chooseInt
  , vectorOf
  )

instance Arbitrary Exp where
  arbitrary = sized (\size -> genExp size [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genVar :: Gen VName
genVar = do
  first <- elements ['a' .. 'z']
  restLength <- chooseInt (1, 3)
  rest <- vectorOf restLength $
    elements (['a' .. 'z'] ++ ['0' .. '9'])
  pure (first : rest)

genUnboundVar :: [VName] -> Gen Exp
genUnboundVar vars = do
  name <- genVar
  if name `elem` vars
    then genUnboundVar vars
    else pure (Var name)

genVariable :: [VName] -> Gen Exp
genVariable [] =
  frequency
    [ (49, CstInt <$> arbitrary)
    , (1, genUnboundVar [])
    ]
genVariable vars =
  frequency
    [ (49, Var <$> elements vars)
    , (1, genUnboundVar vars)
    ]

genLet :: Int -> [VName] -> Gen Exp
genLet size vars = do
  name <- genVar
  value <- genExp (size `div` 2) vars
  body <- genExp (size `div` 2) (name : vars)
  pure (Let name value body)

genLambda :: Int -> [VName] -> Gen Exp
genLambda size vars = do
  name <- genVar
  body <- genExp (size - 1) (name : vars)
  pure (Lambda name body)

genExp :: Int -> [VName] -> Gen Exp
genExp size vars
  | size <= 0 =
      frequency
        [ (3, CstInt <$> arbitrary)
        , (3, CstBool <$> arbitrary)
        , (4, genVariable vars)
        ]

  | otherwise =
      frequency
        [ (2, CstInt <$> arbitrary)
        , (2, CstBool <$> arbitrary)
        , (3, genVariable vars)
        , (2, Add <$> sub <*> sub)
        , (2, Sub <$> sub <*> sub)
        , (2, Mul <$> sub <*> sub)
        , (5, Div <$> sub <*> sub)
        , (5, Pow <$> sub <*> sub)
        , (2, Eql <$> sub <*> sub)
        , (2, If <$> third <*> third <*> third)
        , (3, genLet size vars)
        , (3, genLambda size vars)
        , (2, Apply <$> sub <*> sub)
        , (2, TryCatch <$> sub <*> sub)
        ]

  where
    sub = genExp (size `div` 2) vars
    third = genExp (size `div` 3) vars

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted _ = undefined

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e = case runEval (eval e) of
  Left err -> err `elem` checkExp e
  Right _ -> True

-- The number of tests is part of the specification of this test suite: some of
-- these properties fail only rarely.  Do not reduce it.
properties :: [(String, Property)]
properties =
  [ ("expCoverage", property $ withMaxSuccess 10000 expCoverage)
  , ("parsePrinted", property $ withMaxSuccess 10000 parsePrinted)
  , ("onlyCheckedErrors", property $ withMaxSuccess 10000 onlyCheckedErrors)
  ]
