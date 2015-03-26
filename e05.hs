--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
main = do
  putStrLn "E05: Smallest Multiple"
  putStrLn ("Test case (should be 2520):" ++ show (smallestTen))
  putStrLn ("Solution? " ++ show (smallestTwenty))

oneToTen = [1..10]
allDivByTen = filter (isDivByList oneToTen) [2,4..]
smallestTen = head allDivByTen

oneToTwenty = [1..20]
allDivByTwety = filter (isDivByList oneToTwenty) [2,4..]
smallestTwenty = head allDivByTwety -- long running

isDivByList list x = foldr (&&) True (map (\z -> x `mod` z == 0) list)



