module Renkon.Util
  ( module Renkon.Util
  , module X
  ) where

import ClassyPrelude

import Control.Monad.IO.Class
import Data.Text as Text
import Data.Text.Lazy.Builder as Text
import Formatting
import System.Console.ANSI
import System.Console.ANSI.Types as X
import System.Directory
import System.Process


-- * System utilities

-- | Look for an executable from @PATH@.
which :: Text -> IO (Maybe FilePath)
which = findExecutable . Text.unpack

-- | Execut a command.
execute :: FilePath -> [Text] -> IO ()
execute exe args = callProcess exe $ Text.unpack <$> args

-- | Execut a command and returns stdio.
execute' :: FilePath -> [Text] -> IO Text
execute' exe args = do
  (_, Just hout, _, _) <- createProcess (proc exe args') { std_out = CreatePipe }
  decodeUtf8 <$> hGetContents hout
  where
    args' = Text.unpack <$> args

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

-- | @ln@ formatter
ln :: Format r r
ln = "\n"

-- | @indent@ formatter
indent :: Int -> Format r r
indent n = now $ Text.fromText $ Text.replicate n " "
