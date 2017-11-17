{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

import Test.Hspec
import RPNAST


prob1 :: String -> PExp
prob1 (x:xs)
     | x == ' ' = prob1(xs) 
     | x == '*' && length(xs) > 0 = [Mul]  ++ prob1(xs)
     | x == '-' && length(xs) > 0 = [Minus]  ++ prob1(xs)
     | x == '+' && length(xs) > 0 = [Plus]  ++ prob1(xs)
     | x == '/' && length(xs) > 0 = [IntDiv]  ++ prob1(xs)
     | x == '*' && length(xs) == 0 = [Mul]  
     | x == '-' && length(xs) == 0 = [Minus]  
     | x == '+' && length(xs) == 0 = [Plus]  
     | x == '/' && length(xs) == 0 = [IntDiv]
     | otherwise && length(xs) > 0 = [Val (read (x:xs) :: Int)]     



prob2    :: a
prob2    = undefined

prob3 :: PExp -> RPNResult
prob3 a = hstack a []
    where
        hstack :: PExp -> [Int] -> RPNResult
        hstack ((Val i):rest) vals      = hstack rest (i:vals)
        hstack (Plus:rest) (r:l:vals)   = hstack rest ((l + r):vals)
        hstack (Minus:rest) (r:l:vals)  = hstack rest ((l - r):vals)
        hstack (Mul:rest) (r:l:vals)    = hstack rest ((l * r):vals)
        hstack (IntDiv:rest) (0:l:vals) = Failure DivByZero
        hstack (IntDiv:rest) (r:l:vals) = hstack rest ((l `div` r):vals)
        hstack [] [i]                   = Success i
        hstack _ _                      = Failure BadSyntax

prob4    :: a
prob4    = undefined

-- Write your Hspec Tests below
