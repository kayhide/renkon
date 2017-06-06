{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Renkon.Util
  ( module Renkon.Util
  , module X
  ) where

import System.Console.ANSI
import System.Console.ANSI.Types as X
import Turtle


ln :: (a ~ b) => Format a b
ln = "\n"

withColor :: (MonadIO m) => ColorIntensity -> Color -> m () -> m ()
withColor intencity color action = do
  liftIO $ setSGR [SetColor Foreground intencity color]
  action
  liftIO $ setSGR [SetColor Foreground Vivid White]

withBold :: (MonadIO m) => m () -> m ()
withBold action = do
  liftIO $ setSGR [SetConsoleIntensity BoldIntensity]
  action
  liftIO $ setSGR [SetConsoleIntensity NormalIntensity]
