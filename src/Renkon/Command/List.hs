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
  bin' <- reader (^. path . renkonBin)
  exists <- testdir $ bin'
  when (not exists) $ do
    withColor Vivid Red $ do
      printf ("renkon root does not exist." % ln)
    withColor Vivid White $ do
      stdout $ toLines bin' & indent 2
    exit ExitSuccess
  echo "Available generators:"
  withColor Vivid Green $ do
    stdout $ ls bin' >>= toRelative bin' >>= toLines & indent 2
