{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.List
  ( run
  ) where

import Data.Maybe
import Data.List as List
import Data.Text as Text
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens.Operators
import System.FilePath
import System.FilePath.Find as FilePath
import System.Directory
import Formatting

import Renkon.Util
import Renkon.Config


run :: Config -> IO ()
run config = do
  let root' = config ^. path . renkonRoot
      bin' = config ^. path . renkonBin

  onDirAbsence root' $ do
    withColor Red $ do
      fprint ("renkon root does not exist." % ln)
    withColor White $ do
      fprint ("  " % string % ln) root'
    guard False

  fprint ("Available generators:" % ln)

  path' <- getSearchPath
  gens' <- sequence $ findRenconGenerator config <$> path'
  withColor Green $ do
    mapM_ (displayList config) gens'

displayList :: Config -> [String] -> IO ()
displayList config xs = mapM_ print' xs
  where
    print' = fprint (indent 2 % string % ln) . stripPrefix' . takeBaseName
    stripPrefix' x = fromMaybe x $ List.stripPrefix pre' x
    pre' = Text.unpack $ config ^. path . renkonPrefix

isRenconBinDir :: Config -> FindClause Bool
isRenconBinDir config = List.isPrefixOf pre' <$> filePath
  where
    pre' = config ^. path . renkonBin

isRenconGenerator :: Config -> FindClause Bool
isRenconGenerator config = List.isPrefixOf pre' <$> fileName
  where
    pre' = Text.unpack $ config ^. path . renkonPrefix

findRenconGenerator :: Config -> FilePath -> IO [FilePath]
findRenconGenerator config dir = do
  exists <- doesDirectoryExist dir
  if exists
  then FilePath.find (filePath ==? dir) (isRenconGenerator config) dir
  else return []


onDirAbsence :: (MonadIO m) => FilePath -> m () -> m ()
onDirAbsence dir action = do
  exists <- liftIO $ doesDirectoryExist dir
  when (not exists) $ do
    action
