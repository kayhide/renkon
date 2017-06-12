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
import Renkon.Command.Path as PathCommand
import Renkon.Command.Generate as GenerateCommand


start :: IO ()
start = do
  run_ $
    Group "Generator manager"
    [ subCmd "list" list'
    , subCmd "path" path'
    , subCmd "generate" generate'
    ]

list'
  :: Flag "d" '["detail"] "" "show detail infomation" Bool
  -> Cmd "List available generators" ()
list' detail = liftIO $ do
  let detail' = get detail
  config <- boot
  ListCommand.run config detail'

path' :: Cmd "Display path information" ()
path' = liftIO $ do
  config <- boot
  PathCommand.run config

generate'
  :: Arg "<GENERATOR>" String
  -> Arg "[ARGS...]" [String]
  -> Cmd "Launch the generator" ()
generate' generator args = liftIO $ do
  let generator' = Text.pack $ get generator
      args' = Text.pack <$> get args
  config <- boot
  GenerateCommand.run config generator' args'
