--examples from class to help
-- by AnnaKathleen
-- for Spooketober

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

-- set building

-- [ x^2+1 | x <- [1, 2, 3, 4, 5, 6, 7] ]
	-- = [2,5,10,17,26,37,50]

-- modified with guards, x is even

-- [ x^2+1 | x <- [1, 2, 3, 4, 5, 6, 7], even x ]

-- = [5,17,37]

--[ succ c | c <- "hello" ]
 	-- = "ifmmp"

-- using .. notation

firstSquares n = [ i*i | i <- [1..n] ]
firstEvenSquares n = [ i*i | i <- [1..n], even i]
firstEvenSquares' n = [ i*i | i <- [2,4..n]]

-- a quicksort in four lines!

qs [] = []
qs (x:xs) = smaller ++ [x] ++ larger
  where smaller = qs [a | a<-xs, a<=x]
        larger = qs [a | a<-xs, a>x]

-- another ex of where clause

somePowers x = [x, sq x, sq (sq x)]
  where sq n = n*n 		

 -- ex of the let clause

qs' [] = []
qs' (x:xs) =
  let smaller = qs' [a | a<-xs, a<=x]
      larger = qs' [a | a<-xs, a>x]
  in smaller ++ [x] ++ larger

-- ex for recursion
-- calculate powers ( x^y for x, y integers, y >= 0 ).
-- uses the fact the x^0 = 1 and x^y = x * x^(y-1)
-- runs O(y)

myPower _ 0 = 1
myPower x y = x * myPower x (y-1)

-- a more efficient implementation using fact that x^y = (x^(y/2))^2
-- runs O(logy)
myPower' x y
  | y==0      = 1
  | even y    = half*half
  | odd y     = x*half*half
  where half = myPower' x (div y 2)

-- examples of partially applied function (these are equiv)
myConcat :: [[a]] -> [a]
myConcat xs = foldl (++) [] xs

myConcat' :: [[a]] -> [a]
myConcat' = foldl (++) [] --part. app.

-- using partially applied functions to pass a function as an argument
divisors n = filter (divides n) [2..(n `div` 2)]
  where divides a b = (a `mod` b) == 0

-- lambda expressions
addToEach :: Num a => a -> [a] -> [a]
addToEach n lst = map (\x -> x+n) lst

-- ex of lambda increasing readability
commajoin0:: [String] -> String
commajoin0 = \xs -> intercalate "," xs

-- uncurried function
myDiv :: (Int, Int) -> Int
myDiv (n,d) = div n d

-- ex of how to use uncurried:
-- useful during list manipulations, particularly with zip, which puts two lists together into tuples:
addPairwise :: Num a => [a] -> [a] -> [a]
addPairwise xs ys = map (uncurry (+)) (zip xs ys)

-- function composition examples
hailstone n
  | even n = n `div` 2
  | odd n = (3 * n) + 1

hailSeq :: Int -> [Int]
hailSeq 1 = [1]
hailSeq n = n : hailSeq(hailstone n)

hailLen :: Int -> Int
hailLen = length . hailSeq

reverseJoin :: String -> [String] -> String
reverseJoin s = (intercalate s) . reverse

weekday :: Day -> Int
weekday = snd . mondayStartWeek

-- join func
-- shown as intercalate

prependToAll :: a -> [a] -> [a]
prependToAll _ [] = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse sep (x:xs) = x : prependToAll sep xs

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

-- where concat = concatenate a list of lists

--concat :: [[a]] -> [a]
--concat = foldr (++) []

commajoin :: [String] -> String
commajoin = \xs -> intercalate "," xs

 -- a good pascal's triangle implementation for exer5
pascal :: Integer -> [Integer]
pascal 0 = [1]
pascal n = zipWith (+) ([0] ++ pascal (n-1)) (pascal (n-1) ++ [0])
