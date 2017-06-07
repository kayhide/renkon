{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.Path where

import Control.Monad
import Control.Monad.Reader
import Control.Lens.Operators
import Turtle

import Renkon.Util
import Renkon.Config


run :: ReaderT Config Shell ()
run = do
  config <- reader id
  printf ("renkon-root: " % fp % ln) $ config ^. path . renkonRoot
  printf ("renkon-bin: " % fp % ln) $ config ^. path . renkonBin
