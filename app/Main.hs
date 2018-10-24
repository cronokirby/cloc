module Main where

import System.Environment (getArgs)

import Cloc (run)

main :: IO ()
main = do
    args <- getArgs
    run args
