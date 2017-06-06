{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.List where

import Control.Monad
import Control.Lens.Operators
import Turtle

import Renkon.Config

run :: Config -> IO ()
run config = sh $ do
  exists <- liftIO $ testfile $ config ^. path . renkonRoot
  when (not exists) $ do
    printf (fp % " does not exist.\n") $ config ^. path .renkonRoot
  guard exists
  echo "Available generators:"
  view $ ls $ config ^. path . renkonRoot
