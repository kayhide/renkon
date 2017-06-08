{-# LANGUAGE OverloadedStrings #-}

module Renkon.Cli
  ( start
  ) where

import Data.Text
import Control.Monad.Reader
import Turtle

import Renkon.Config
import Renkon.Command
import Renkon.Command.List as ListCommand
import Renkon.Command.Path as PathCommand
import Renkon.Command.Generate as GenerateCommand


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
  command <- options "Renkon generators manager" parser
  config <- boot
  runCommand config command

runCommand :: Config -> Command -> IO ()
runCommand config ListCommand =
  sh $ flip runReaderT config ListCommand.run

runCommand config PathCommand =
  sh $ flip runReaderT config PathCommand.run

runCommand config (GenerateCommand generator args) = do
  sh $ flip runReaderT (config, generator, args) GenerateCommand.run