{-# LANGUAGE OverloadedStrings #-}

module Renkon
  ( start
  ) where

import Data.Text
import Turtle

import Renkon.Config
import Renkon.Command.List as ListCommand
import Renkon.Command.Path as PathCommand


data Command = ListCommand | PathCommand | GenerateCommand Text (Maybe Text)
  deriving (Show)

listParser :: Parser Command
listParser = subcommand "list" "List the generators available" $ pure ListCommand

pathParser :: Parser Command
pathParser = subcommand "path" "Display path information" $ pure PathCommand

generateParser :: Parser Command
generateParser = subcommand "generate" "Run the generator" $
  GenerateCommand
  <$> argText "generator" "The generator"
  <*> optional (optText "args" 'a' "Arguments for the generator")

parser :: Parser Command
parser = listParser <|> pathParser <|> generateParser

start :: IO ()
start = do
  x <- options "Renkon generators manager" parser
  config <- boot
  case x of
    ListCommand -> ListCommand.run config
    PathCommand -> PathCommand.run config
    GenerateCommand generator args -> do
      printf ("Launching " % s % " generator...\n") generator
      printf ("Args: " % w % "\n") args
