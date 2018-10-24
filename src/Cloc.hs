module Cloc
    ( run
    ) where

import qualified Data.Text.IO as T
import Pipes
import qualified System.IO as IO

import TextPipes


run :: FilePath -> IO ()
run fp = IO.withFile fp IO.ReadMode $ \h ->
    runEffect (printLines h)
  where
    printLines h = for (textLines h) (liftIO . T.putStrLn)
