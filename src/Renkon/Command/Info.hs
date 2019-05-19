module Renkon.Command.Info
  ( run
  ) where

import ClassyPrelude

import Control.Lens.Operators
import Formatting
import Renkon.Config
import Renkon.Util


run :: Config -> Text -> IO ()
run config generator = do
  exe <- which $ sformat (stext % stext) (config ^. path . renkonPrefix) generator

  case exe of
    Nothing -> do
      withColor Red $
        fprint ("generator is not found." % ln)
      withVivid White $
        fprint ("  " % stext % ln) generator
    Just exe' ->
      displayItemDetail config exe'

displayItemDetail :: Config -> FilePath -> IO ()
displayItemDetail config exe = do
  withColor Green $
    fprint (indent 2 % string % ln) . takeGeneratorName config $ exe
  withColor Yellow $
    fprint (indent 2 % string % ln) exe
  withColor White $
    traverse_ (fprint (indent 4 % stext % ln)) . lines =<< execute' exe ["--help"]
  fprint ln
