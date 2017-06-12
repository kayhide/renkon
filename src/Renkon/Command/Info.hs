{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.Info
  ( run
  ) where

import Data.Maybe
import Data.List as List
import Data.Text as Text
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens.Operators
import System.FilePath
import Formatting

import Renkon.Util
import Renkon.Config


run :: Config -> Text -> IO ()
run config generator = do
  exe <- which $ sformat (stext % stext) (config ^. path . renkonPrefix) generator

  case exe of
    Nothing -> do
      withColor Red $ do
        fprint ("generator is not found." % ln)
      withVivid White $ do
        fprint ("  " % stext % ln) generator
    Just exe' -> do
      displayItemDetail config exe'

displayItemDetail :: Config -> FilePath -> IO ()
displayItemDetail config exe = do
  withColor Green $ do
    fprint (indent 2 % string % ln) . (takeGeneratorName config) $ exe
  withColor Yellow $ do
    fprint (indent 2 % string % ln) exe
  withColor White $ do
    mapM_ (fprint (indent 4 % string % ln)) . List.lines =<< execute' exe ["--help"]
  fprint ln
