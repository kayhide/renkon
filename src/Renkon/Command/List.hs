{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.List where

import Control.Lens.Operators
import Turtle

import Renkon.Config

run :: Config -> IO ()
run config = sh $ do
  echo "Generators available:"
  view $ ls $ config ^. path . renkonRoot
  return ()
