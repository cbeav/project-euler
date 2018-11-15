module ProjectEuler (runProblem) where

import Prelude
import Data.Char (digitToInt, ord)
import Data.List (intercalate, maximumBy, sort)
import Data.Maybe
import Data.Numbers.Primes
import Data.Set (toList)
import qualified Data.Text as T
import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Powers.Squares
import Unsafe.Coerce

runProblem :: String -> IO ()
runProblem problem = case problem of
  "1" -> p1
  "2" -> p2
  "3" -> p3
  "4" -> p4
  "5" -> p5
  "6" -> p6
  "7" -> p7
  "8" -> p8
  "9" -> p9
  "10" -> p10
  "13" -> p13
  "14" -> p14
  "15" -> p15
  "16" -> p16
  "17" -> p17
  "20" -> p20
  "21" -> p21
  "22" -> p22
  p -> putStrLn $ "Unrecognized problem: " ++ p

p1 :: IO ()
p1 = print $ sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

fibs :: [Integer]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

p2 :: IO ()
p2 = print . sum . takeWhile (< 4000000) $ filter even fibs

p3 :: IO ()
p3 = print . maximum $ primeFactors (600851475143 :: Integer)

isPalindrome :: Integer -> Bool
isPalindrome i = show i == reverse (show i)

p4 :: IO()
p4 = print $ maximum [x*y | x <- [999,998..100], y <- [999,998..100], isPalindrome (x*y)]

p5 :: IO () -- dedup prime factorizations of [1..20]
p5 = print $ product [2, 2, 5, 19, 3, 3, 17, 2, 2, 7, 13, 11]

p6 :: IO ()
p6 = print $ squareOfSum - sumOfSquares
  where
    sumOfSquares = sum $ map (^2) [1..100]
    squareOfSum = (^2) $ sum [1..100]

p7 :: IO ()
p7 = print $ primes !! 10000

p8 :: IO ()
p8 = do
  num <- intercalate "" . lines <$> readFile "resources/p8.txt"
  print $ check num 0
 where
  check str mx
    | length str < 13 = mx
    | otherwise = check (tail str) (max mx (prod $ take 13 str))
  prod str = product $ map (toInteger . digitToInt) str

p9 :: IO ()
p9 =
  print . prod3 $ head [(a, b, c) | a <- [1..1000 :: Integer], b <- [1..500], c <- maybeToList (exactSquareRoot (a^2 + b^2)), isSquare' (a^2 + b^2), (a + b + c) == 1000]
 where
  prod3 (a, b, c) = a * b * c

p10 :: IO ()
p10 = print . sum $ takeWhile (< 2000000) primes

p13 :: IO ()
p13 = do
  nums <- map read . lines <$> readFile "resources/p13.txt"
  print . take 10 . show $ sum nums

collatz :: Integer -> Integer
collatz n
  | even n = floor (toRational n / 2)
  | otherwise = 3 * n + 1

collatzLength :: Integer -> Integer
collatzLength 1 = 1
collatzLength n = 1 + collatzLength (collatz n)

p14 :: IO ()
p14 =
  print . maximumBy compVals $ map (\a -> (a, collatzLength a)) [1..999999]
 where
  compVals (_, v1) (_, v2) = compare v1 v2

p15 :: IO ()
p15 =
  let
    nextRow prevRow = zipWith (+) (0 : nextRow prevRow) prevRow
    dpTable = take 21 (replicate 21 1 : map nextRow dpTable)
  in
    print . last $ last dpTable

sumd :: Integer -> Integer -> Integer
sumd 0 acc = acc
sumd x acc = sumd (x `div` 10) (acc + (x `mod` 10))

p16 :: IO ()
p16 = print $ sumd (2 ^ 1000) 0

writeNumber :: Integer -> String
writeNumber n
  | n == 0 = ""
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | n == 10 = "ten"
  | n == 11 = "eleven"
  | n == 12 = "twelve"
  | n == 13 = "thirteen"
  | n `elem` [14, 16, 17, 19] = writeNumber (n `mod` 10) ++ "teen"
  | n == 15 = "fifteen"
  | n == 18 = "eighteen"
  | n `elem` [20..29] = "twenty" ++ writeNumber (n `mod` 10)
  | n `elem` [30..39] = "thirty" ++ writeNumber (n `mod` 10)
  | n `elem` [40..49] = "forty" ++ writeNumber (n `mod` 10)
  | n `elem` [50..59] = "fifty" ++ writeNumber (n `mod` 10)
  | n `elem` [60..69] = "sixty" ++ writeNumber (n `mod` 10)
  | n `elem` [70..79] = "seventy" ++ writeNumber (n `mod` 10)
  | n `elem` [80..89] = "eighty" ++ writeNumber (n `mod` 10)
  | n `elem` [90..99] = "ninety" ++ writeNumber (n `mod` 10)
  | n == 1000 = "onethousand"
  | mod n 100 == 0 = writeNumber (div n 100) ++ "hundred"
  | otherwise = writeNumber (div n 100) ++ "hundredand" ++ writeNumber (mod n 100)

p17 :: IO ()
p17 = print . sum $ map (length . writeNumber) [1..1000]

p20 :: IO ()
p20 = print $ sumd (product [1..100]) 0

p21 :: IO ()
p21 =
  print . sum $ filter isAmicable ([2..9999] :: [Int])
 where
  isAmicable n = n == sumDivisors (sumDivisors n) && sumDivisors n /= n
  sumDivisors n = (\a -> a - n) . sum . toList $ divisors n

p22 :: IO ()
p22 = do
  names <- map T.unpack . T.splitOn "," . T.pack . filter (/= '"') <$> readFile "resources/p22.txt"
  print . sum $ zipWith (*) (map nameVal $ sort names) [1..]
 where
  nameVal = sum . map ((\a -> a - 64) . ord)
