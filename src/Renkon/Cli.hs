{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Renkon.Cli
  ( start
  ) where

import Data.Text as Text
import Control.Monad.IO.Class
import Options.Declarative as Options

import Renkon.Config
import Renkon.Command.List as ListCommand
import Renkon.Command.Info as InfoCommand
import Renkon.Command.Path as PathCommand
import Renkon.Command.Generate as GenerateCommand


start :: IO ()
start = do
  run_ $
    Group "Generator manager"
    [ subCmd "list" list'
    , subCmd "info" info'
    , subCmd "generate" generate'
    , subCmd "path" path'
    ]

list'
  :: Flag "d" '["detail"] "" "show detail infomation" Bool
  -> Cmd "List available generators" ()
list' detail = liftIO $ do
  let detail' = get detail
  config <- boot
  ListCommand.run config detail'

info'
  :: Arg "<GENERATOR>" String
  -> Cmd "Display detail information of the generator" ()
info' generator = liftIO $ do
  let generator' = Text.pack $ get generator
  config <- boot
  InfoCommand.run config generator'

generate'
  :: Arg "<GENERATOR>" String
  -> Arg "[ARGS...]" [String]
  -> Cmd "Launch the generator" ()
generate' generator args = liftIO $ do
  let generator' = Text.pack $ get generator
      args' = Text.pack <$> get args
  config <- boot
  GenerateCommand.run config generator' args'

path' :: Cmd "Display path information" ()
path' = liftIO $ do
  config <- boot
  PathCommand.run config
