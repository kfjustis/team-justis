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
