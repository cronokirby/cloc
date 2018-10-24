{-# LANGUAGE OverloadedStrings #-}
module Cloc
    ( run
    ) where

import Control.Monad (forM, forM_)
import Data.Foldable (foldrM)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pipes
import qualified Pipes.Prelude as P
import qualified System.IO as IO

import TextPipes


textShow :: Show s => s -> T.Text
textShow = T.pack . show

-- | Represents a counted file along with filepath and line count
data CountElement = CountElement FilePath Int


totalCount :: [CountElement] -> Int
totalCount = foldr go 0
  where
    go (CountElement _ i) acc = i + acc

prettyCount :: CountElement -> T.Text
prettyCount (CountElement fp i)  =
    T.pack fp <> ": " <> textShow i <> " lines"


countLines :: FilePath -> IO CountElement
countLines fp = IO.withFile fp IO.ReadMode $ \h -> do
    let lines = textLines h
    lineCount <- P.length lines
    return (CountElement fp lineCount)

run :: [FilePath] -> IO ()
run files = do
    counts <- forM files countLines
    let total = totalCount counts
        totalTxt = "\nTotal Lines: " <> textShow total
    forM_ counts (T.putStrLn . prettyCount)
    T.putStrLn totalTxt
