{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

import Test.Hspec
import RPNAST


prob1   :: String -> PExp
prob1 a = map (stringop :: String -> Op) (words a)

--This function is inspired by work done by Ryan Lapeyre
stringop :: String -> Op
stringop (x:xs)
    | x == '*' = Mul
    | x == '-' = Minus
    | x == '+' = Plus
    | x == '/' = IntDiv
    | otherwise = (Val (read (x:xs) :: Int))


prob2    :: a
prob2    = undefined

prob3    :: a
prob3    = undefined

prob4    :: a
prob4    = undefined

-- Write your Hspec Tests below
