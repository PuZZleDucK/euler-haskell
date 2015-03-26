
module Main where
import Test.QuickCheck

{-
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
-}
main :: IO ()
main = do
  putStrLn " :: Euler problem 03: Fibbos Sum ::"
--  putStrLn (" :: Prime factors: " ++ (show (primeFactorsOf 60081)))
--  putStrLn (" :: Prime factors: " ++ (show (primeFactorsOf 600851475143)))
--  putStrLn (" :: Largest: " ++ (show (foldl max 0 (primeFactorsOf 600851475143))))
                                                               --   8462696833... incorrect
  putStrLn "Checking:"
--  putStrLn $ "factors of 4:" ++ (show (primeFactorsOf 4))
--  quickCheck prop_factors
--  putStrLn $ "pFactorsOf 1:" ++ (show (pFactorsOf 1))
--  putStrLn $ "pFactorsOf 2:" ++ (show (pFactorsOf 2))
--  putStrLn $ "pFactorsOf 3:" ++ (show (pFactorsOf 3))
  putStrLn $ "pFactorsOf 4:" ++ (show (pFactorsOf 4))
  putStrLn $ "pFactorsOf 5:" ++ (show (pFactorsOf 5))
  putStrLn $ "pFactorsOf 6:" ++ (show (pFactorsOf 6))
  putStrLn $ "pFactorsOf 7:" ++ (show (pFactorsOf 7))
  putStrLn $ "pFactorsOf 8:" ++ (show (pFactorsOf 8))
  putStrLn $ "pFactorsOf 9:" ++ (show (pFactorsOf 9))
  putStrLn $ "pFactorsOf10:" ++ (show (pFactorsOf 10))
  putStrLn $ "pFactorsOf12:" ++ (show (pFactorsOf 12))
  
  quickCheck prop_factors2
  putStrLn $ "pFactorsOf 13195:" ++ (show (pFactorsOf 13195))
  putStrLn (" :: Prime factors of 600851475143: " ++ (show (pFactorsOf 600851475143)))


pFactorsOf :: Integer -> [Integer]
pFactorsOf n | n <= 3 = [n]
             | divisor /= 0 = divisor:(pFactorsOf (n `div` divisor))
             | otherwise = [n]
  where divisor = primeMax n


prime :: Integer -> Bool
prime n | length (divisors n) == 0 = True
        | otherwise = False

divisors :: Integer -> [Integer]
divisors x = [z | z <- [2..(x-1)], x `mod` z == 0]

primeDivisors :: Integer -> [Integer]
primeDivisors x = [z | z <- [(x-1),(x-2)..2], x `mod` z == 0 && prime z] --not that different :(
--primeDivisors x = [z | z <- [2..(x-1)], x `mod` z == 0 && prime z] -- ~8 sec benchmark


primeMax :: Integer -> Integer -- returns highest divisor or 0
primeMax n | length (divs) == 0 = 0
           | otherwise = head (divs)
  where divs = primeDivisors n

prop_factors2 :: Integer -> Property
prop_factors2 x = x > 0 ==> product (pFactorsOf x) == x


{-
prop_factors :: Integer -> Property
prop_factors x = x > 0 ==> product (primeFactorsOf x) == x

  
divisorsOf :: Integer -> [Integer]
divisorsOf 0 = []
divisorsOf n = n:filter (\x -> ((n `mod` x)==0)) [1..(n `div` 2)]
  
isPrime :: Integer -> Bool
isPrime n
  | n==2 = True
  | even n = False
  | otherwise = (length (divisorsOf n)) == 2

primeFactorsOf ::  Integer -> [Integer]
primeFactorsOf n = filter isPrime (divisorsOf n)
-}


