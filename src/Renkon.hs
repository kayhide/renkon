{-# LANGUAGE OverloadedStrings #-}

module Renkon
  ( start
  ) where

import Turtle


data Command = ListCommand | GenerateCommand Text (Maybe Text)
  deriving (Show)

listParser :: Parser Command
listParser = subcommand "list" "List the generators available" $ pure ListCommand

generateParser :: Parser Command
generateParser = subcommand "generate" "Run the generator" $
  GenerateCommand
  <$> argText "generator" "The generator"
  <*> optional (optText "args" 'a' "Arguments for the generator")

parser :: Parser Command
parser = listParser <|> generateParser

start :: IO ()
start = do
  x <- options "Renkon generators manager" parser
  print x
  case x of
    ListCommand -> printf ("Hello, "%s%"!\n") "world"
    GenerateCommand generator args -> do
      printf ("Launching " % s % " generator...\n") generator
      printf ("Args: " % w % "\n") args
