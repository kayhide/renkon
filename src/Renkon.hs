{-# LANGUAGE OverloadedStrings #-}

module Renkon
  ( start
  ) where

import Turtle

import Renkon.Config
import Renkon.Command.Data
import Renkon.Command.List as ListCommand


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
  config <- boot
  print x
  print config
  case x of
    ListCommand -> ListCommand.run config
    GenerateCommand generator args -> do
      printf ("Launching " % s % " generator...\n") generator
      printf ("Args: " % w % "\n") args
