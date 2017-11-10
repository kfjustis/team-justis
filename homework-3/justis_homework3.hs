{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

import Test.Hspec
import RPNAST


prob1 :: String -> PExp
prob1 (s) =
    if length (s) == 1 then
        if s == '+' then
            [Plus]
        else if s == '-' then
            [Minus]
        else if s == '*' then
            [Mul]
        else if s == '/' then
            [IntDiv]
    else
        --let a:ab = words(x:xs)
        --in prob1 (a) ++ [ab]
        let x:xs = words s
        in prob1 x ++ xs

prob2 :: a
prob2 = undefined

prob3 :: a
prob3 = undefined

prob4 :: a
prob4 = undefined

-- Write your Hspec Tests below
