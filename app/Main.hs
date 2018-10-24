module Main where

import System.Environment (getArgs)

import Cloc (run)

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> putStrLn "Please give me a file to print out"
        (f:_) -> run f
