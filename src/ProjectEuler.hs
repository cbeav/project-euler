module ProjectEuler where

import Prelude
import Data.Char (digitToInt)
import Data.List (intercalate)
import Data.Maybe
import Data.Numbers.Primes
import Math.NumberTheory.Powers.Squares
import Unsafe.Coerce

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
