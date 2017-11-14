{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

import Test.Hspec
import RPNAST


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

prob2 :: PExp -> Int
--prob2 [Val a] = a
{-prob2 (x:xs) =
    if length (x:xs) >= 1 then
        if (x == Val) then
            x
    else
        0-}

prob3 :: a
prob3 = undefined

prob4 :: a
prob4 = undefined

-- Write your Hspec Tests below
