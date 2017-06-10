{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.Path where

import Control.Monad
import Control.Monad.Reader
import Control.Lens.Operators
import Formatting

import Renkon.Util
import Renkon.Config


run :: Config -> IO ()
run config = do
  fprint ("renkon-root: " % string % ln) $ config ^. path . renkonRoot
  fprint ("renkon-bin: " % string % ln) $ config ^. path . renkonBin
