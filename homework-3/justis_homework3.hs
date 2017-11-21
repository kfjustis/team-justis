{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

import Test.Hspec
import RPNAST

import Control.Exception -- this seems required for using evaluate in prob2 tests and so on...

-- Based on Nathan's implementation
prob1 :: String -> PExp
prob1 a = map stringToOp (words a)
  where
    stringToOp :: String -> Op
    stringToOp (x:xs)
        | x == '*' = Mul
        | x == '-' = Minus
        | x == '+' = Plus
        | x == '/' = IntDiv
        | otherwise = (Val (read (x:xs) :: Int))

-- Shout out to Tom for the help
prob2 :: PExp -> Int
prob2 a = hstack a []
    where
        hstack :: PExp -> [Int] -> Int
        hstack ((Val i):rest) vals      = hstack rest (i:vals)
        hstack (Plus:rest) (r:l:vals)   = hstack rest ((l + r):vals)
        hstack (Minus:rest) (r:l:vals)  = hstack rest ((l - r):vals)
        hstack (Mul:rest) (r:l:vals)    = hstack rest ((l * r):vals)
        hstack (IntDiv:rest) (0:l:vals) = errorWithoutStackTrace "Cannot divide by 0"
        hstack (IntDiv:rest) (r:l:vals) = hstack rest ((l `div` r):vals)
        hstack [] [i]                   = i
        hstack _ _                      = errorWithoutStackTrace "Unexpected case encountered"

-- Ryan taught be how to use constructors
prob3 :: PExp -> RPNResult
prob3 a = hstack' a []
    where
        hstack' :: PExp -> [Int] -> RPNResult
        hstack' ((Val i):rest) vals       = hstack' rest (i:vals)
        hstack' (Plus:rest) (r:l:vals)    = hstack' rest ((l + r):vals)
        hstack' (Minus:rest) (r:l:vals)   = hstack' rest ((l - r):vals)
        hstack' (Mul:rest) (r:l:vals)     = hstack' rest ((l * r):vals)
        hstack' (IntDiv:rest) (0:l:vals)  = Failure DivByZero
        hstack' (IntDiv:rest) (r:l:vals)  = hstack' rest ((l `div` r):vals)
        hstack' [] [i]                    = Success i
        hstack' _ _                       = Failure BadSyntax

-- Shout out to Nathan and Tom for the extra help over break
prob4 :: PExp -> Result String String
prob4 a = bstr a []
    where
        bstr :: PExp -> [String] -> Result String String
        bstr ((Val i):rest) str         = bstr rest ((show i):str)
        bstr (Plus:rest) (r:l:ops)      = bstr rest (("(" ++ l ++ " + " ++ r ++ ")"):ops)
        bstr (Minus:rest) (r:l:ops)     = bstr rest (("(" ++ l ++ " - " ++ r ++ ")"):ops)
        bstr (Mul:rest) (r:l:ops)       = bstr rest (("(" ++ l ++ " * " ++ r ++ ")"):ops)
        bstr (IntDiv:rest) ("0":l:ops)  = Failure "DivByZero"
        bstr (IntDiv:rest) (r:l:ops)    = bstr rest (("(" ++ l ++ " / " ++ r ++ ")"):ops)
        bstr [] [x]                     = Success x
        bstr _ _                        = Failure "BadSyntax"

-- Write your Hspec Tests below
test_prob1 :: IO ()
test_prob1 = hspec $ do
    describe "prob1" $ do
        it "returns [Plus] when given \"+\"" $
            prob1 "+" `shouldBe` [Plus]
        it "returns [Val 100] when given \"100\"" $
            prob1 "100" `shouldBe` [Val 100]
        it "returns [Val 200, Plus, Minus, Mul, IntDiv] when given \"200 + - * /\"" $
            prob1 "200 + - * /" `shouldBe` [Val 200, Plus, Minus, Mul, IntDiv]

test_prob2 :: IO ()
test_prob2 = hspec $ do
    describe "prob2" $ do
        context "when dividing by 0" $ do
            it "should return an error" $ do
                evaluate (prob2 [Val 4, Val 0, IntDiv] :: Int) `shouldThrow` anyException
        context "when encountering Bad Syntax" $ do
            it "should return an error" $ do
                evaluate (prob2 [Mul]) `shouldThrow` anyException
        context "when using first example input" $ do
            it "returns 2 when given [Val 4, Val 2, IntDiv]" $
                prob2 [Val 4, Val 2, IntDiv] `shouldBe` 2

test_prob3 :: IO ()
test_prob3 = hspec $ do
    describe "prob3" $ do
        it "returns Success 10 when given [Val 5, Val 1, Val 1, Plus, Mul]" $
            prob3 [Val 5, Val 1, Val 1, Plus, Mul] `shouldBe` Success 10
        it "returns Failure DivByZero when given [Val 5, Val 0, IntDiv]" $
            prob3 [Val 5, Val 0, IntDiv] `shouldBe` Failure DivByZero
        it "returns Failure BadSyntax when given [IntDiv, Plus, Val 0]" $
            prob3 [IntDiv, Plus, Val 0] `shouldBe` Failure BadSyntax

test_prob4 :: IO ()
test_prob4 = hspec $ do
    describe "prob4" $ do
        it "returns Success \"(1 + 1)\" when given [Val 1, Val 1, Plus]" $ do
            prob4 [Val 1, Val 1, Plus] `shouldBe` Success "(1 + 1)"
        it "returns Success \"((2 + 4) / 3)\" when given [Val 2, Val 4, Plus, Val 3, IntDiv]" $ do
            prob4 [Val 2, Val 4, Plus, Val 3, IntDiv] `shouldBe` Success "((2 + 4) / 3)"
        it "returns Success \"2\" when given [Val 2]" $ do
            prob4 [Val 2] `shouldBe` Success "2"
        it "returns Failure \"BadSyntax\" when given [Plus]" $ do
            prob4 [Plus] `shouldBe` Failure "BadSyntax"

-- Thanks Tom for Homework 1 and Homework 2
test_probs :: IO ()
test_probs = do
   test_prob1
   test_prob2
   test_prob3
   test_prob4
