module Homework2 where
import Test.QuickCheck


-- Function prob1
-- @type
-- @param
-- @output
{- @description:
    Re-writing listComp using the functions map and filter. map takes in a function and a list.
    filter takes in a predicate and a list. So we check for an empty list, and if its not empty
    we give the results from filter p xs to map to use with the function f.

    Written by Calvin Wallace
-}
-- listComp f p xs = [ f x | x <- xs, p x]
<<<<<<< HEAD
prob1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
prob1 f c xs = map f (filter c xs)
=======
-- prob1 :: a
-- prob1 = undefined
prob1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
prob1 _ _ [] = []
prob1 f p xs = map f (filter p xs)


>>>>>>> 991fc69d0d3853913b5d6b27f898436565c59e07
-- Function prob2
-- @type
-- @param
-- @output
<<<<<<< HEAD
-- @description:
prob2 :: Integer -> [Integer]
prob2 x 
    |x < 0 = []
    |x < 10 = [x]
    |otherwise = prob2 (x `div` 10) ++ [x `mod` 10]
=======
{- @description:
    Example: 332
    1) 332 div 10 -> 33
       332 mod 10 -> [2]
    2) 33 div 10 -> 3
       33 mod 10 -> [3] -> [3, 2]
    3) 3 div 10 -> 0 -> prob2 (0) -> []
       3 mod 10 -> 3 -> [3, 3, 2]
    Final: [] ++ [3, 3, 2] -> [3, 3, 2]

    Basically, for values over 10, we recursively divide by 10. This value is used to determine
    if we need to divide again. If not, we've reached some value < 0 and cons the empty list, or
    we reach a list with a single value. Before that however, we mod the initial value with 10
    to return the remainder. This gets added to the list immediately because we know that each digit
    should be some value less than 10. For the final result, the terminated left hand side is cons'd
    with the list that was built up on the right hand side.

    Source: https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell

    Written by Ryon Siebel
-}
prob2 :: Integer -> [Integer]
prob2 num
    | num < 0   = []
    | num < 10  = [ num ]
    | otherwise = prob2 ( num `div` 10 ) ++ [ num `mod` 10 ]


>>>>>>> 991fc69d0d3853913b5d6b27f898436565c59e07
-- Function prob3
-- @type
-- @param
-- @output
<<<<<<< HEAD
-- @description:
=======
{- @description:
    Example:
    1) (123)
    2) 3:(12)
    3) 3:2:(1)
    4) 3:2:1

    This works the same way as problem 2 except instead of appending items to the end of a list,
    it works by always appending the remainder to the head of the list.

    Written by Nathan Stahl
-}
>>>>>>> 991fc69d0d3853913b5d6b27f898436565c59e07
prob3 :: Integer -> [Integer]
prob3 x
    |x < 0 = []
    |x < 10 = [x]
    |otherwise = [x `mod` 10] ++ prob2(x `div` 10)
<<<<<<< HEAD
=======


>>>>>>> 991fc69d0d3853913b5d6b27f898436565c59e07
-- Function prob4
-- @type
-- @param
-- @output
<<<<<<< HEAD
-- @description:
--prob4 :: a
--prob4 = undefined
prob4 :: [Integer] -> [Integer]
prob4 [] = []
prob4 (x:xs)
    | length(x:xs) `mod` 2 == 0 = [2*x] ++ prob4 xs
    | otherwise = [x] ++ prob4 xs
-- Function prob5
-- @type   
-- @param  
-- @output
-- @description:
prob5 :: [Integer] -> Integer
prob5 [] = 0
prob5 (x:xs)
    | x >= 10         = prob5 (prob2 x ++ xs)
    | x > 0 && x < 10 = x + prob5 xs
    | otherwise = 0 + prob5 xs
=======
{- @description:
    Looks at each element of the passed list. If the current list it is looking at is even in length,
    then me want to multiply the left most value and append that value to the new list. If it's not,
    we just take the head and append it to the new list leaving it untouched. This continues for the
    whole list resulting in a new list where every other value is multiplied by two from the right.

    Written by Kynan Justis
-}
prob4 :: [Integer] -> [Integer]
prob4 [] = []
prob4 (x:xs) =
    if (length (x:xs)) `mod` 2 == 0 then
        [x * 2] ++ prob4 xs
    else
        [x] ++ prob4 xs
>>>>>>> 991fc69d0d3853913b5d6b27f898436565c59e07


-- Function prob5
-- @type
-- @param
-- @output
{- @description:
    This also runs through each element in the list and does a check. If the head of the list is between
    0-9, then it adds that value to the sum and runs the function again on the rest of the list. If the head
    is greater than 10, then we pass it off to prob2 to separate its digits and append that smaller list to the
    end of the original list. With that new list, we recursively call prob5 again until all the values in the first
    list have been processed, which leaves us with a sum of the digits.

    Written by Kynan Justis
-}
prob5 :: [Integer] -> Integer
prob5 [] = 0
prob5 (x:xs)
    | x > 0 && x < 10 = x + prob5 xs
    | x >= 10         = prob5 (xs ++ prob2 x)
    | otherwise       = 0 + prob5 xs


---------------------------------------------
--               Unit Tests                --
---------------------------------------------
test_prob1 :: IO ()
test_prob1  = do
  putStrLn "Problem 1 Results:"
  prob1_test1
  prob1_test2
test_prob2 :: IO ()
test_prob2  = do
  putStrLn "Problem 2 Results:"
  prob2_test1
  prob2_test2
test_prob3 :: IO ()
test_prob3  = do
  putStrLn "Problem 3 Results:"
  prob3_test1
test_prob4 :: IO ()
test_prob4  = do
  putStrLn "Problem 4 Results:"
  prob4_test1
  prob4_test2
test_prob5 :: IO ()
test_prob5  = do
  putStrLn "Problem 5 Results:"
  prob5_test1
test_probs :: IO ()
test_probs  = do
  putStrLn "-------- All Problem Results --------"
  test_prob1
  test_prob2
  test_prob3
  test_prob4
  test_prob5
  putStrLn "-------------------------------------"
prob1_test1 = quickCheckWith (stdArgs {maxSuccess = 1000}) prob1_property1
  where
    prob1_property1 :: [Integer] -> Bool
    prob1_property1 xs = lComp (+1) (even) xs == prob1 (+1) (even) xs
      where lComp f p xs = [ f x | x <- xs, p x]
prob1_test2 = quickCheckWith (stdArgs {maxSuccess = 1000}) prob1_property2
  where
    prob1_property2 :: [Int] -> Bool
    prob1_property2 xs = lComp (*2) (odd) xs == prob1 (*2) (odd) xs
      where lComp f p xs = [ f x | x <- xs, p x]
prob2_test1 = quickCheckWith (stdArgs {maxSuccess = 1000}) prob2_property1
  where
    prob2_property1 :: Integer -> Bool
    prob2_property1 xs = abs xs == (go1 . prob2) (abs xs)
      where go1 :: [Integer] -> Integer
            go1 xs
              | (null xs) = 0
              | otherwise = let pos = filter (> -1) xs
                            in  read (foldl (++) "" $ map show pos) :: Integer
prob2_test2 = quickCheckWith (stdArgs {maxSuccess = 1000}) prob2_property2
  where
    prob2_property2 :: Integer -> Bool
    prob2_property2 xs  = prob2 xs == prob2_dual xs
      where prob2_dual :: Integer -> [Integer]
            prob2_dual x
              | x < 0      = []
              | otherwise  = map go' $ show x
              where go' '0' = 0
                    go' '1' = 1
                    go' '2' = 2
                    go' '3' = 3
                    go' '4' = 4
                    go' '5' = 5
                    go' '6' = 6
                    go' '7' = 7
                    go' '8' = 8
                    go' '9' = 9
prob3_test1 = quickCheckWith (stdArgs {maxSuccess = 1000}) prob3_property1
  where
    prob3_property1 :: Integer -> Bool
    prob3_property1 xs  = prob3 xs == go1 xs
      where go1 :: Integer -> [Integer]
            go1 n | n < 0      = []
                  | otherwise  = reverse $ map go' $ show n
              where go' '0' = 0
                    go' '1' = 1
                    go' '2' = 2
                    go' '3' = 3
                    go' '4' = 4
                    go' '5' = 5
                    go' '6' = 6
                    go' '7' = 7
                    go' '8' = 8
                    go' '9' = 9
prob4_test1 = quickCheckWith (stdArgs {maxSuccess = 1000}) prob4_property1
  where
    prob4_property1 :: Integer -> Integer -> Integer -> Integer -> Bool
    prob4_property1 w x y z = [(w + w),x, (y + y), z] == prob4 [w,x,y,z]
prob4_test2 = quickCheckWith (stdArgs {maxSuccess = 1000}) prob4_property2
  where
    prob4_property2 :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
    prob4_property2 v w x y z = [v, (w + w), x, (y + y), z] == prob4 [v,w,x,y,z]
prob5_test1 = quickCheckWith (stdArgs {maxSuccess = 1000}) prob5_property
  where
    prob5_property :: [Integer] -> Bool
    prob5_property xs = prob5 (map (abs) xs) == go' xs
    go' :: [Integer] -> Integer
    go' is = go1 (map (abs) is) 0
      where go1 :: [Integer] -> Integer -> Integer
            go1 [] n     = n
            go1 (x:xs) n | (x < 10)   = go1 xs (x + n)
                         | (x > 9)    = go1 xs ((sum (go2 x)) + n)
                         | otherwise  = go1 xs n
            go2 :: Integer -> [Integer]
            go2 x
              | x < 0      = []
              | otherwise  = map go3 $ show x
            go3 :: Char -> Integer
            go3 '0' = 0
            go3 '1' = 1
            go3 '2' = 2
            go3 '3' = 3
            go3 '4' = 4
            go3 '5' = 5
            go3 '6' = 6
            go3 '7' = 7
            go3 '8' = 8
            go3 '9' = 9
