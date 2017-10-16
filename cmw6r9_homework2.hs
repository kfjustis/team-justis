module Homework2 where
import Test.QuickCheck
-- Function prob1
-- @type
-- @param f a function, p a function that returns a Bool, and xs, a list of appropriate type
-- @output
-- @description: rewriting listComp using the functions map and filter. map takes in a function and a list. Filter takes in a predicate and a list. So we check for an empty list, and if its not empty we give the results from filter p xs to map to use with the function f.
-- listComp f p xs = [ f x | x <- xs, p x]
prob1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
prob1 _ _ [] = []
prob1 f p xs = map f (filter p xs)
-- Function prob2
-- @type
-- @param
-- @output
-- @description:
--how to split a number into digits haskell ->
--https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
--    135 `div` 10 = 13
--    135 `mod` 10 = 5
-- check if x is negative, then check if x is less than 10, if its not use mod 10 on x to get the remainder and add it to the list, then use div 10 to iterate through the number
prob2 :: Integer -> [Integer]
prob2 x
    | x < 0 = []
    | x < 10 = [x]
    | otherwise = prob2 (x `div` 10) ++ [x `mod` 10]
-- Function prob3
-- @type
-- @param
-- @output
-- @description: basically the same as prob2 but reversed. Takes in an Integer as an input and returns a list of digits in reverse.
prob3 :: Integer -> [Integer]
prob3 x
    | x < 0 = []
    | x < 10 = [x]
    | otherwise = x `mod` 10 : prob3 (x `div` 10)
-- Function prob4
-- @type
-- @param
-- @output
-- @description: If you love arrays use length
-- takes a list of non-negative numbers and multiplies every other digit starting from the right by 2
-- essentially, get the length of the list and use the mod operator on it to see if the length of the list is an even number. If the list is even then we can start multiplying by the first element of the list. Otherwise just add it to the list without multiplication.
{- Adapted from
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs)
  |  (length (x:y:zs)) `mod` 2    /= 0    =    x : y*2 : doubleEveryOther zs
  |  otherwise                            =    x*2 : y : doubleEveryOther zs
  https://stackoverflow.com/questions/19867491/double-every-other-element-of-list-from-right-in-haskell
  -}
prob4 :: [Integer] -> [Integer]
prob4 [] = []
prob4 (x:xs)
    | length (x:xs) `mod` 2 == 0  = [x * 2] ++ prob4 xs
    | otherwise                   =  [x] ++ prob4 xs
-- Function prob5
-- @type
-- @param
-- @output
-- @description: in class [18, 1, 2, 18] = [1,8,1,2,1,8] use recursion with problem 2
-- initial check to see if empty list, if not, check if x is between 0 and 10 (ie a digit 0-9). if it is add that value to the recursive call on the rest of the list. if x >10 (like 18 in the example) we want to break it into a list of two elements so we call prob2 to break it into multiple digits. Then we recursively sum those digits with the rest of the list. Otherwise, the number is negative, so we are going to treat negative numbers as 0s.
prob5 :: [Integer] -> Integer
prob5 [] = 0
prob5 (x:xs)
    | x >= 0 && x < 10     = x + prob5 xs
    | x > 10              = prob5(prob2(x)) + prob5 xs
    | otherwise           = 0 + prob5 xs






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
