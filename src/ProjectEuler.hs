module ProjectEuler (runProblem) where

import Prelude
import Data.Char (digitToInt)
import Data.List (intercalate, maximumBy)
import Data.Maybe
import Data.Numbers.Primes
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
