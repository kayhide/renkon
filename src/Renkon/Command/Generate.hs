{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.Generate
  ( run
  ) where

import Data.Maybe
import Data.Text as Text
import Control.Monad
import Control.Lens.Operators
import System.Process
import Formatting

import Renkon.Util
import Renkon.Config


-- | Run generate command.
run :: Config -> Text -> [Text] -> IO ()
run config generator args = do
  gen <- which $ sformat (stext % stext) (config ^. path . renkonPrefix) generator

  case gen of
    Nothing -> do
      withColor Red $ do
        fprint ("generator is not found." % ln)
      withVivid White $ do
        fprint ("  " % stext % ln) generator
    Just gen' -> do
      fprint "Launching "
      withBold Green $ do
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
