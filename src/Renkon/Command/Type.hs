module Renkon.Command.Type where

import Data.Text


data Command = ListCommand | PathCommand | GenerateCommand Text (Maybe Text)
  deriving (Show)
