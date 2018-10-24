{-# LANGUAGE RankNTypes #-}
{-|
Description: Contains functions for working with textual pipes
-}
module TextPipes
    ( textLines
    )
where

import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pipes
import qualified Pipes.Prelude as P
import qualified System.IO as IO


textLines :: MonadIO m => IO.Handle -> Producer' T.Text m ()
textLines h = go
  where
    go = do
        eof <- liftIO $ IO.hIsEOF h
        unless eof $ do
            txt <- liftIO $ T.hGetLine h
            yield txt
            go
