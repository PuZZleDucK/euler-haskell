--Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

module Main where

main = do
  putStrLn "Project Euler problem 6: Sum square difference"
  putStrLn $ "Ten Delta: " ++ (show tenDelta)
  putStrLn $ "Hundred Delta: " ++ (show ((hundredDelta)))



firstTenNats = [1..10]
tenSumOfSquares = foldr (+) 0 $ map (** 2) firstTenNats
tenSquareOfSums = (foldr (+) 0 firstTenNats) ** 2
tenDelta = tenSquareOfSums - tenSumOfSquares

firstHundredNats = [1..100]
hundredSumOfSquares = foldr (+) 0 $ map (** 2) firstHundredNats
hundredSquareOfSums = (foldr (+) 0 firstHundredNats) ** 2
hundredDelta = hundredSquareOfSums - hundredSumOfSquares


--The sum of the squares of the first ten natural numbers is,
--12 + 22 + ... + 102 = 385

--The square of the sum of the first ten natural numbers is,
--(1 + 2 + ... + 10)2 = 552 = 3025

--Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.




