module Main where

import ClassyPrelude
import ProjectEuler

main :: IO ()
main = do
  (problem:_) <- getArgs
  case problem of
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
    _ -> putStrLn $ "Unrecognized problem: " ++ problem
