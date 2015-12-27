
--

--A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
--a^2 + b^2 = c^2

--For example, 32 + 42 = 9 + 16 = 25 = 52.

--There exists exactly one Pythagorean triplet for which a + b + c = 1000.
--Find the product abc. 200*375*425


target = 1000

main = do
  putStrLn "Euler 9: Special Pythagorean triplet"
  let candidates = findCandidates
  putStrLn $ "candidates: " ++ (show (length candidates))
  let pythagCandidates = findPythags candidates
  putStrLn $ "pythag candidates: " ++ (show (length pythagCandidates))
  let pythagSolution = multiplyTriplet (head (pythagCandidates))
  putStrLn $ "solution(s): " ++ (show pythagSolution)


multiplyTriplet :: (Integer,Integer,Integer) -> Integer
multiplyTriplet (x,y,z) = x*y*z

checkTarget :: (Integer,Integer,Integer) -> Bool
checkTarget (x,y,z) = x*x + y*y == z*z

findPythags :: [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)]
findPythags candidates = do
  let pythagCandidates = filter checkTarget candidates
  pythagCandidates

findCandidates :: [(Integer,Integer,Integer)]
findCandidates = do
  let candidates = [(x,y,z) | x <- [1..target], y <- [x..target], z <- [y..target], x + y + z == target]
--  "V: " ++ (show candidates)
  candidates



