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


-- | Represents the line counts for some file
data LineCount = LineCount
    { codeLines :: Int
    , commentLines :: Int
    }

instance Semigroup LineCount where
    LineCount c1 cm1 <> LineCount c2 cm2 = LineCount (c1 + c2) (cm1 + cm2)

instance Monoid LineCount where
    mempty = LineCount 0 0
    mappend = (<>)


-- | Represents a counted file along with filepath and line count
data CountElement = CountElement 
    { file :: FilePath
    , count :: LineCount
    }

totalCount :: [CountElement] -> LineCount
totalCount = foldr go mempty
  where
    go (CountElement _ i) acc = i <> acc

prettyCount :: CountElement -> T.Text
prettyCount (CountElement fp l)  =
    let code = textShow (codeLines l)
        comment = textShow (commentLines l)
    in T.pack fp 
        <> ": " 
        <> code <> " lines ("
        <> comment <> " comments)"


-- | Represents a predicate that is true if the line is a comment
newtype LineFilter = LineFilter (T.Text -> Bool)


isComment :: LineFilter
isComment = LineFilter (T.isPrefixOf "--")

countLines :: FilePath -> IO CountElement
countLines fp = IO.withFile fp IO.ReadMode $ \h -> do
    let lines = textLines h
    lineCount <- P.fold (go isComment) mempty id lines
    return (CountElement fp lineCount)
  where
    go :: LineFilter -> LineCount -> T.Text -> LineCount
    go (LineFilter f) (LineCount code comment) line =
        if f line
            then LineCount code (comment + 1)
            else LineCount (code + 1) comment

run :: [FilePath] -> IO ()
run files = do
    counts <- forM files countLines
    let LineCount total _ = totalCount counts
        totalTxt = "\nTotal Lines: " <> textShow total
    forM_ counts (T.putStrLn . prettyCount)
    T.putStrLn totalTxt
