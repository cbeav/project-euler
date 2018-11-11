module Main where

import Prelude
import ProjectEuler
import System.Environment (getArgs)

main :: IO ()
main = runProblem =<< (head <$> getArgs)
