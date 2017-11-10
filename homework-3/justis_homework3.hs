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




prob1    :: a
prob1    = undefined

prob2    :: a
prob2    = undefined

prob3    :: a
prob3    = undefined

prob4    :: a
prob4    = undefined

-- Write your Hspec Tests below
