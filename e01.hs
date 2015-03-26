
module Main where
{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000.
-}
main = do
  putStrLn " :: Euler problem 01: 3s and 5s ::"
  putStrLn (" :: Sum of mults of 3&5 < 10: " ++ (show (sum (multiplesOfLessThan [3,5] 10))))
  putStrLn (" :: Sum of mults of 3&5 < 10: " ++ (show (sum (multiplesOfLessThan [3,5] 1000))))
  
  

numbers :: [Integer]
numbers = numbers' 1

numbers' :: Integer -> [Integer]
numbers' x = [x]++(numbers' ((fromInteger x)+1))

multiplesOfLessThan :: [Integer] -> Integer -> [Integer]
multiplesOfLessThan divisors upTo = multiplesOfLessThan' 1 divisors upTo

multiplesOfLessThan' :: Integer -> [Integer] -> Integer -> [Integer]
multiplesOfLessThan' thisNum divisors upTo
  | upTo == thisNum = []
  | any (==0) (fmap (thisNum `mod`) [3,5]) = [thisNum]++(multiplesOfLessThan' (thisNum+1) divisors upTo)
  | otherwise = (multiplesOfLessThan' (thisNum+1) divisors upTo)
