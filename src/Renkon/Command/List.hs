{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.List where

import Prelude hiding (FilePath)
import Data.Function
import Data.Maybe
import Control.Monad
import Control.Monad.Reader
import Control.Lens.Operators
import Turtle

import Renkon.Util
import Renkon.Config

run :: (MonadIO m) => ReaderT Config m ()
run = do
  bin' <- reader (^. path . renkonBin)
  exists <- liftIO $ testdir $ bin'
  when (not exists) $ do
    printf (fp % " does not exist." % ln) $ bin'
    exit ExitSuccess
  echo "Available generators:"
  stdout $ getBinFiles bin' >>= toLines

getBinFiles :: FilePath -> Shell FilePath
getBinFiles bin' = do
  file <- ls bin'
  let file' = stripPrefix bin' file
  guard $ isJust file'
  return $ fromJust file'

toLines :: FilePath -> Shell Line
toLines file = do
  let line = textToLine $ format fp $ file
  guard $ isJust line
  return $ fromJust line
