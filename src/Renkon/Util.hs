{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Renkon.Util
  ( module Renkon.Util
  , module X
  ) where

import Data.Maybe
import Data.Text as Text
import Control.Monad.IO.Class
import System.Environment
import System.FilePath
import System.Directory
import System.Process
import System.Console.ANSI
import System.Console.ANSI.Types as X
import Formatting


-- * System utilities

-- | Look for an executable from @PATH@.
which :: Text -> IO (Maybe FilePath)
which = findExecutable . Text.unpack

-- | Execut a command.
execute :: FilePath -> [Text] -> IO ()
execute exe args = callProcess exe $ Text.unpack <$> args


-- * Coloring

-- | Run IO action with colored output.
withColor :: (MonadIO m) => Color -> m () -> m ()
withColor color action = do
  liftIO $ setSGR [SetColor Foreground Dull color]
  action
  liftIO $ setSGR [Reset]

-- | Run IO action with high-intencity colored output.
withVivid :: (MonadIO m) => Color -> m () -> m ()
withVivid color action = do
  liftIO $ setSGR [SetColor Foreground Vivid color]
  action
  liftIO $ setSGR [Reset]

-- | Run IO action with bold colored output.
withBold :: (MonadIO m) => Color -> m () -> m ()
withBold color action = do
  liftIO $ setSGR [ SetColor Foreground Vivid color
                  , SetConsoleIntensity BoldIntensity
                  ]
  action
  liftIO $ setSGR [Reset]


-- * Formatting

-- | @ln@ formatter for formatting
ln :: Format r r
ln = "\n"
