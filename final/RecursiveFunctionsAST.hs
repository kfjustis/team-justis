module RecursiveFunctionsAST where

import Prelude hiding (LT, GT, EQ)
import Operators

data Exp = Literal    Value
         | Unary      UnaryOp Exp
         | Binary     BinaryOp Exp Exp
         | If         Exp Exp Exp
         | Variable   String
         | Declare    [(String,Exp)] Exp
         | RecDeclare String Exp Exp
         | Function   String Exp     
         | Call       Exp Exp        
  deriving (Eq, Show)

data Value = IntV  Int
           | BoolV Bool
           | ClosureV String Exp Env
  deriving (Eq, Show)

type Env = [(String, Value)]


unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)

binary Add (IntV a)  (IntV b)  = IntV (a + b)
binary Sub (IntV a)  (IntV b)  = IntV (a - b)
binary Mul (IntV a)  (IntV b)  = IntV (a * b)
binary Div (IntV a)  (IntV b)  = IntV (a `div` b)
binary And (BoolV a) (BoolV b) = BoolV (a && b)
binary Or  (BoolV a) (BoolV b) = BoolV (a || b)
binary LT  (IntV a)  (IntV b)  = BoolV (a < b)
binary LE  (IntV a)  (IntV b)  = BoolV (a <= b)
binary GE  (IntV a)  (IntV b)  = BoolV (a >= b)
binary GT  (IntV a)  (IntV b)  = BoolV (a > b)
binary EQ  a         b         = BoolV (a == b)
