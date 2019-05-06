module Renkon.Command.List
  ( run
  ) where

import Control.Lens.Operators
import Control.Monad
import Control.Monad.IO.Class
import Data.List as List
import Data.Text as Text
import Formatting
import Renkon.Config
import Renkon.Util
import System.Directory
import System.FilePath
import System.FilePath.Find as FilePath


run :: Config -> Bool -> IO ()
run config detail = do
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
  let displayItem = if detail then displayItemDetail else displayItemSimple
  mapM_ (displayList (displayItem config)) gens'


displayList :: (FilePath -> IO ()) -> [FilePath] -> IO ()
displayList displayItem xs = mapM_ displayItem xs

displayItemSimple :: Config -> FilePath -> IO ()
displayItemSimple config exe = do
  withColor Green $ do
    fprint (indent 2 % string % ln) . (takeGeneratorName config) $ exe

displayItemDetail :: Config -> FilePath -> IO ()
displayItemDetail config exe = do
  withColor Green $ do
    fprint (indent 2 % string % ln) . (takeGeneratorName config) $ exe
  withColor Yellow $ do
    fprint (indent 2 % string % ln) exe
  withColor White $ do
    mapM_ (fprint (indent 4 % string % ln)) . List.lines =<< execute' exe ["--help"]
  fprint ln

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
