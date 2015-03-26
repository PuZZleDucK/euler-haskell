--Problem 4: Largest palindrome product
import Test.QuickCheck
--TestCase:
--The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
--Find the largest palindrome made from the product of two 3-digit numbers.

main :: IO ()
main = do
  putStrLn ":: E04 - Palendromic Products (Solved) ::"
  putStrLn ("Two Digit: " ++ (show (maxTwoStringPalendrome twoDigitStringPalendromes (0,("","")))))
  putStrLn ("Three Digit: " ++ (show (maxThreeStringPalendrome threeDigitStringPalendromes (0,("","")))))


--two digit
twoDigitStrings :: [String]
twoDigitStrings = [x:y:[] | x<-['1'..'9'], y<-['0'..'9']]

twoDigitStringPairs = [(x,y) | x<-twoDigitStrings, y<-twoDigitStrings]

twoDigitStringProducts :: [(Integer,(String,String))]
twoDigitStringProducts = [((read (fst pair))*(read (snd pair)),pair) | pair<-twoDigitStringPairs]

twoDigitStringPalendromes = filter (\x -> isPalendromeString (fst x)) twoDigitStringProducts

maxTwoStringPalendrome :: [(Integer,(String,String))] -> (Integer,(String,String)) -> (Integer,(String,String))
maxTwoStringPalendrome [] max = max
maxTwoStringPalendrome (x:xs) max = if (fst x) < (fst max)
  then maxTwoStringPalendrome xs max
  else maxTwoStringPalendrome xs x

--three digit
threeDigitStrings :: [String]
threeDigitStrings = [x:y:z:[] | x<-['1'..'9'], y<-['0'..'9'], z<-['0'..'9']]

threeDigitStringPairs = [(x,y) | x<-threeDigitStrings, y<-threeDigitStrings]

threeDigitStringProducts :: [(Integer,(String,String))]
threeDigitStringProducts = [((read (fst pair))*(read (snd pair)),pair) | pair<-threeDigitStringPairs]

threeDigitStringPalendromes = filter (\x -> isPalendromeString (fst x)) threeDigitStringProducts

maxThreeStringPalendrome :: [(Integer,(String,String))] -> (Integer,(String,String)) -> (Integer,(String,String))
maxThreeStringPalendrome [] max = max
maxThreeStringPalendrome (x:xs) max = if (fst x) < (fst max)
  then maxThreeStringPalendrome xs max
  else maxThreeStringPalendrome xs x

--utils:
isPalendromeString :: Integer -> Bool
isPalendromeString x = isPal intString
  where intString = (show x) :: String

isPal :: String -> Bool
isPal [] = True
isPal [x] = True
isPal (x:xs) = if x == last xs then isPal (dropLast xs) else False

dropLast :: String -> String
dropLast [x] = []
dropLast (x:xs) = x:(dropLast xs)











-- First attempt used integers...
-- "09" is throwing me off, as it comes out as 9... might need to use strings
twoDigitPalendromes = filter (\x -> isPalendrome (fst x)) twoDigitProducts
threeDigitPalendromes = filter (\x -> isPalendrome (fst x)) threeDigitProducts

twoDigitProducts = [ ((fst pair)*(snd pair),pair) | pair <- twoDigitPairs]
threeDigitProducts = [ ((fst pair)*(snd pair),pair) | pair <- threeDigitPairs]

twoDigitPairs = [(x,y) | x <- twoDigitNumbers, y <- twoDigitNumbers]
threeDigitPairs = [(x,y) | x <- threeDigitNumbers, y <- threeDigitNumbers]

twoDigitNumbers = [10..99]
threeDigitNumbers = [100..999]



isPalendrome :: Integer -> Bool
isPalendrome x | x < 10 = True --single digit is pal
               | x < 100 = if (units x) == (unitOfMsd x) then True else False --two digits, only true if equal
               | otherwise = if (units x) == (unitOfMsd x) --more than two, chech match and pass on
  then isPalendrome (stripMsdAndLsd x)
  else False 


units :: Integer -> Integer
units x = x `mod` 10
unitOfMsd :: Integer -> Integer
unitOfMsd x = if x >= 10 then unitOfMsd (x `div` 10) else x
distanceOfMsd :: Integer -> Integer -> Integer
distanceOfMsd x count = if x > 10 then distanceOfMsd (x `div` 10) (count+1) else count

stripMsdAndLsd :: Integer -> Integer
stripMsdAndLsd x = ((x `mod` exp) `div` 10)
  where exp = 10 ^ ((distanceOfMsd x 0))







prop_is_pal1 :: Integer -> Property
prop_is_pal1 x = (x < 10) ==> isPalendrome x == True
prop_is_pal2 :: Integer -> Property
prop_is_pal2 x = (x < 10) ==> isPalendrome (x+(10*x)) == True
prop_is_pal3 :: Integer -> Property
prop_is_pal3 x = (x < 10) ==> isPalendrome (x+(10*x)+(100*x)) == True
prop_is_pal4 :: Integer -> Integer -> Property
prop_is_pal4 x y = ((x > 0)&&(y > 0)&&(x < 10)&&(y < 10)) ==> isPalendrome (x+(10*y)+(100*x)) == True

