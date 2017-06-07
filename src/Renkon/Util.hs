{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Renkon.Util
  ( module Renkon.Util
  , module X
  ) where

import Prelude hiding (FilePath)
import Data.Maybe
import qualified Data.Text as Text
import System.Console.ANSI
import System.Console.ANSI.Types as X
import Turtle


ln :: (a ~ b) => Format a b
ln = "\n"

withColor :: (MonadIO m) => ColorIntensity -> Color -> m () -> m ()
withColor intencity color action = do
  liftIO $ setSGR [SetColor Foreground intencity color]
  action
  liftIO $ setSGR [Reset]

withBold :: (MonadIO m) => m () -> m ()
withBold action = do
  liftIO $ setSGR [SetConsoleIntensity BoldIntensity]
  action
  liftIO $ setSGR [Reset]


indent :: Int -> Shell Line -> Shell Line
indent n = sed $ (pre' <>) <$> (plus dot)
  where pre' = Text.replicate n " "

toRelative :: FilePath -> FilePath -> Shell FilePath
toRelative base path = do
  guard $ isJust path'
  return $ fromJust path'
  where path' = stripPrefix base path

toLines :: (ToDisplayText a) => a -> Shell Line
toLines = select . textToLines . toDisplayText


class ToDisplayText a where
  toDisplayText :: a -> Text
  default toDisplayText :: Show a => a -> Text
  toDisplayText = Text.pack . show

instance ToDisplayText Text where
  toDisplayText = id

instance ToDisplayText FilePath where
  toDisplayText = format fp

instance ToDisplayText Int
