module RPNAST (
   Op(..)
  ,PExp(..)
  ,RPNError(..)
  ,Result(..)
  ,RPNResult(..)
              ) where

data Op           = Val Int
                  | Plus
                  | Minus
                  | Mul
                  | IntDiv
                  deriving (Show, Eq)

type PExp         = [Op]

data RPNError     = DivByZero
                  | BadSyntax
                  deriving (Show, Eq)

data Result a b   = Failure a
                  | Success b
                  deriving (Show, Eq)

type RPNResult    = Result RPNError Int
