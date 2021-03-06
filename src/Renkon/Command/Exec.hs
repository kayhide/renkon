module Renkon.Command.Exec
  ( run
  ) where

import ClassyPrelude

import Control.Lens.Operators
import Formatting
import Renkon.Config
import Renkon.Util


-- | Run exec command.
run :: Config -> Text -> [Text] -> IO ()
run config generator args = do
  gen <- which $ sformat (stext % stext) (config ^. path . renkonPrefix) generator

  case gen of
    Nothing -> do
      withColor Red $
        fprint ("generator is not found." % ln)
      withVivid White $
        fprint ("  " % stext % ln) generator
    Just gen' -> do
      fprint "Launching "
      withBold Green $
        fprint stext generator
      fprint (" generator..." % ln)
      launch gen' args


launch :: FilePath -> [Text] -> IO ()
launch exe args = do
  withColor White $ do
    fprint ("  exe: " % string % ln) exe
    fprint ("  args: " % shown % ln) args
  fprint ln
  execute exe args
