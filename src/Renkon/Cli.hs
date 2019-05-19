module Renkon.Cli
  ( start
  ) where

import ClassyPrelude

import Options.Declarative as Options
import Renkon.Command.Exec as ExecCommand
import Renkon.Command.Info as InfoCommand
import Renkon.Command.List as ListCommand
import Renkon.Command.Path as PathCommand
import Renkon.Config


start :: IO ()
start =
  run_ $
    Group "Generator manager"
    [ subCmd "list" list'
    , subCmd "info" info'
    , subCmd "exec" exec'
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
  let generator' = pack $ get generator
  config <- boot
  InfoCommand.run config generator'

exec'
  :: Arg "<GENERATOR>" String
  -> Arg "[ARGS...]" [String]
  -> Cmd "Launch the generator" ()
exec' generator args = liftIO $ do
  let generator' = pack $ get generator
      args' = pack <$> get args
  config <- boot
  ExecCommand.run config generator' args'

path' :: Cmd "Display path information" ()
path' = liftIO $ do
  config <- boot
  PathCommand.run config
