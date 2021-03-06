{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Final where

import Prelude hiding (LT, GT, EQ)
import System.IO
import Base
import Data.Maybe
import Data.List
import Operators
import RecursiveFunctionsAST
import RecursiveFunctionsParse
import Test.Hspec
import Control.Exception (evaluate,AsyncException(..))
-- Uncomment the following if you choose to do Problem 3.
{-
import System.Environment
import System.Directory (doesFileExist)
import System.Process
import System.Exit
import System.Console.Haskeline
--        ^^ This requires installing haskeline: cabal update && cabal install haskeline
-}

{-
12/6/2017
initial env: []
    (x, 1)
    (y, 2)

    where variables = map fst decls
          expression = map snd decls
          value =   <- for each e in expression eval e env
          newEnv = zip variables values
                                        ^
                                        |
                                        don't forget to add the old env to the end with ++
                                        otherwise exp1 will cause an error

-there is a function that flips arguments

evaluate ( ) `shouldThrow` anyException for test x1 and x4

(execute $ parseExp ("var x = 5;" ++ "var f = function(y) { var y = x * y; function (x) {x + y} };" ++
"var g = f(2);" ++ "g(5)")) == (IntV 15)

results:
you will get unbound error on the first three if you don't add the oldEnv back in
exp1 = 11
exp2 = 16
exp3 = 14
exp4 = variable is unbound
-}


--
-- The parsing function, parseExp :: String -> Exp, is defined for you.
--

facvar   = parseExp ("var fac = function(n) { if (n==0) 1 else n * fac(n-1) };" ++
                   "fac(5)")

facrec   = parseExp ("rec fac = function(n) { if (n==0) 1 else n * fac(n-1) };" ++
                   "fac(5)")

exp1     = parseExp "var a = 3; var b = 8; var a = b, b = a; a + b"
exp2     = parseExp "var a = 3; var b = 8; var a = b; var b = a; a + b"
exp3     = parseExp "var a = 2, b = 7; (var m = 5 * a, n = b - 1; a * n + b / m) + a"
exp4     = parseExp "var a = 2, b = 7; (var m = 5 * a, n = m - 1; a * n + b / m) + a"
-- N.b.,                                                  ^^^ is a free occurence of m (by Rule 2)

-----------------
-- The evaluation function for the recursive function language.
-----------------

eval :: Exp -> Env -> Value
eval (Literal v) env                = v
eval (Unary op a) env               = unary  op (eval a env)
eval (Binary op a b) env            = binary op (eval a env) (eval b env)
eval (If a b c) env                 = let BoolV test = eval a env
                                      in if test then  eval b env else eval c env
eval (Variable x) env               = fromJust x (lookup x env)
  where fromJust x (Just v)         = v
        fromJust x Nothing          = error ("Variable " ++ x ++ " unbound!")
eval (Function x body) env          = ClosureV x body env
-----------------------------------------------------------------
--eval (Declare [(x,exp)] body) env = eval body newEnv           -- This clause needs to be changed.
eval (Declare decls body) env = eval body newEnv
  --where newEnv = (x, eval exp env) : env                       --
  where variables = map fst decls
        expressions = map snd decls
        values = foo expressions [[env]]
        --values = map (eval (head (expressions))) [env] --getting first in list every time
        {--values = map (eval (helper (expressions []))) [env]
            where
                helper :: [Exp] -> [Exp] -> Exp
                helper x:rest str = helper rest x
                helper []--}
        newEnv = zip variables values ++ env

--foo :: [a] -> [b] -> [c]
--foo (x:xs) (y:ys) = map (eval x) y ++ foo xs ys
--foo _ _           = error ("dunno")
--the following is good you just have to generate the values
--eval (Declare decls body) env = eval body newEnv
--  where vars = map newEnv decls
--        values = --for each e in expresion eval e env (probs a diff solution had 2 maps)
--        newEnv = zip vars values

--the following is good you just have to generate the values
--eval (Declare decls body) env = eval body newEnv
--  where vars = map newEnv decls
--        values = values
--        newEnv = zip vars values

--the following is good you just have to generate the values
--eval (Declare decls body) env = eval body newEnv
--  where vars = map newEnv decls
--the underscore is a fill in the blank for a function that gets a list of variable (names?)
--        newEnv = zip vars values
-----------------------------------------------------------------
eval (RecDeclare x exp body) env    = eval body newEnv
  where newEnv = (x, eval exp newEnv) : env
eval (Call fun arg) env = eval body newEnv
  where ClosureV x body closeEnv    = eval fun env
        newEnv = (x, eval arg env) : closeEnv

-- Use this function to run your eval solution.
execute :: Exp -> Value
execute exp = eval exp []
-- Example usage: execute exp1

{-

Hint: it may help to remember that:
   map :: (a -> b) -> [a] -> [b]
   concat :: [[a]] -> [a]
when doing the Declare case.

-}

freeByRule1 :: [String] -> Exp -> [String]
freeByRule1 = undefined

freeByRule2 :: [String] -> Exp -> [String]
freeByRule2 = undefined

---- Problem 3.

repl :: IO ()
repl = do
         putStr "RecFun> "
         iline <- getLine
         process iline

process :: String -> IO ()
process "quit" = return ()
process iline  = do
  putStrLn (show v ++ "\n")
  repl
   where e = parseExp iline
         v = eval e []

--Nathans Hidden Helper Functions
foo :: [Exp] -> [[Env]] -> [Value]
foo (x:xs) (y:ys) = map (eval x) y ++ foo xs ys
foo (x:xs) []     = map (eval x) [] ++ foo xs []
foo _ _           = error ("dunno")

-- Hspec Test Land
test_prob1 :: IO ()
test_prob1 = hspec $ do
    describe "prob1" $ do
        it "returns 11 when given exp1" $
            execute exp1 `shouldBe` IntV 11
        it "returns 16 when given exp2" $
            execute exp2 `shouldBe` IntV 16
        it "returns 14 when given exp3" $
            execute exp3 `shouldBe` IntV 14
        context "when encountering unbound variable" $ do
            it "should return an error when given exp4" $ do
                evaluate (execute exp4) `shouldThrow` anyException
