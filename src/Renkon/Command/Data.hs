module Renkon.Command.Data where

import Data.Text

data Command = ListCommand | GenerateCommand Text (Maybe Text)
  deriving (Show)
