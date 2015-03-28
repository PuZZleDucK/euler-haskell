module Main where
import Test.QuickCheck

prop_runs :: Integer -> Bool
prop_runs = \x -> x==x

prop_prime1 :: Bool
prop_prime1 = makePrime 1 == 2
prop_prime2 = makePrime 2 == 3
prop_prime3 = makePrime 3 == 5
prop_prime4 = makePrime 4 == 7
prop_prime5 = makePrime 5 == 11
prop_prime6 = makePrime 6 == 13

main = do
  putStrLn "E07: 10001st prime"
  quickCheck (prop_runs) -- runs?
  quickCheck (prop_prime1)
  quickCheck (prop_prime2)
  quickCheck (prop_prime3)
  quickCheck (prop_prime4)
  quickCheck (prop_prime5)
  quickCheck (prop_prime6)
  putStrLn $ "6 primes [...13]:" ++ (show (take 6 primes))
  putStrLn $ "10001st prime:" ++ (show (take 1 (drop 10000 primes)))


primes :: [Int]
primes = [(makePrime x) | x <- [1..]]

makePrime :: Int -> Int
makePrime 1 = 2
makePrime 2 = 3
makePrime n = head (filter (\x -> not $ x `isDivByList` (fst (splitAt (n-1) primes)) ) [2..])

isDivByList x ys = or (map (x `isDivBy`) ys)

isDivBy x y = x `mod` y == 0

getPrime :: Integer -> Integer
getPrime x = head (drop ((fromInteger x)-1) primeList)

primeList :: [Integer]
primeList = [x | x <- filter (\x -> not (x `elem` (fst (splitAt (fromInteger x) primeList))) ) [2..]]

--By listing the first six prime numbers: , , , , we can see that the 6th prime is 13.

--What is the 10001st prime number?

