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
    withColor Vivid Red $ do
      printf ("renkon root does not exist." % ln)
    printf ("  " % fp) bin'
    printf ln
    exit ExitSuccess
  echo "Available generators:"
  withColor Vivid Green $ do
    stdout $ getBinFiles bin' >>= toTexts >>= toLines

getBinFiles :: FilePath -> Shell FilePath
getBinFiles bin' = do
  file <- ls bin'
  let file' = stripPrefix bin' file
  guard $ isJust file'
  return $ fromJust file'

toTexts :: FilePath -> Shell Text
toTexts = return . format ("  " % fp)

toLines :: Text -> Shell Line
toLines = select . textToLines

