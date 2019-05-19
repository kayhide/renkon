module Renkon.Command.Path where

import ClassyPrelude

import Control.Lens.Operators
import Formatting
import Renkon.Config
import Renkon.Util


run :: Config -> IO ()
run config = do
  fprint ("renkon-root: " % string % ln) $ config ^. path . renkonRoot
  fprint ("renkon-bin: " % string % ln) $ config ^. path . renkonBin
