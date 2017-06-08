{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.List where

import Prelude hiding (FilePath)
import Data.Maybe
import Control.Monad
import Control.Monad.Reader
import Control.Lens.Operators
import Turtle

import Renkon.Util
import Renkon.Config


run :: ReaderT Config Shell ()
run = do
  root' <- reader (^. path . renkonRoot)
  bin' <- reader (^. path . renkonBin)

  onDirAbsence root' $ do
    withColor Red $ do
      echo "renkon root does not exist."
    withColor White $ do
      stdout $ toLines root' & indent 2
    guard False

  echo "Available generators:"

  onDirAbsence bin' $ do
    guard False

  pre' <- reader (^. path . renkonPrefix)
  withColor Green $ do
    stdout $ ls bin'
      >>= toRelative bin'
      >>= toLines
      & grep (begins (text pre'))
      & trimPrefix pre'
      & indent 2

onDirAbsence :: (MonadIO m) => FilePath -> m () -> m ()
onDirAbsence dir action = do
  exists <- testdir dir
  when (not exists) $ do
    action
