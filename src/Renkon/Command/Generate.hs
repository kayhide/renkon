{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.Generate where

import Data.Maybe
import qualified Data.Text as Text
import Control.Monad
import Control.Monad.Reader
import Control.Lens.Operators
import qualified Control.Foldl as Foldl
import Turtle

import Renkon.Util
import Renkon.Config
import Renkon.Command.Type


run :: ReaderT (Config, Text, Maybe Text) Shell ()
run = do
  (config, generator, args) <- reader id
  gen <- which $ fromText $ format (s % s) (config ^. path . renkonPrefix) generator
  when ((not . isJust) gen) $ do
    withColor Vivid Red $ do
      printf ("generator is not found." % ln)
    withColor Vivid White $ do
      stdout $ toLines generator & indent 2
    exit ExitSuccess
  let gen' = format fp $ fromJust gen
      args' = maybeToList args
      cmd = Text.intercalate " " (gen' : args')
  printf "Launching "
  withBold $ do
    withColor Vivid Green $ do
      printf s generator
  printf (" generator..." % ln)
  withColor Vivid White $ do
    printf ("  bin: " % s % ln) gen'
    printf ("  args: " % w % ln) args'
    printf ("  cmd: " % s % ln) cmd
  printf ln
  void $ shell cmd empty
