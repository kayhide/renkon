{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.Path where

import Control.Monad
import Control.Lens.Operators
import Turtle

import Renkon.Util
import Renkon.Config


run :: Config -> IO ()
run config = sh $ do
  printf ("renkon-root: " % fp % ln) $ config ^. path . renkonRoot
  printf ("renkon-bin: " % fp % ln) $ config ^. path . renkonBin
