module Renkon.Command.List
  ( run
  ) where

import ClassyPrelude

import Control.Lens.Operators
import Formatting
import Renkon.Config
import Renkon.Util
import System.Directory (doesDirectoryExist)
import System.FilePath (getSearchPath)
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
  let displayItem = bool displayItemSimple displayItemDetail detail
  traverse_ (displayList (displayItem config)) gens'


displayList :: (FilePath -> IO ()) -> [FilePath] -> IO ()
displayList displayItem xs = traverse_ displayItem xs

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
    traverse_ (fprint (indent 4 % stext % ln)) . lines =<< execute' exe ["--help"]
  fprint ln

isRenconBinDir :: Config -> FindClause Bool
isRenconBinDir config = isPrefixOf pre' <$> filePath
  where
    pre' = config ^. path . renkonBin

isRenconGenerator :: Config -> FindClause Bool
isRenconGenerator config = isPrefixOf pre' <$> fileName
  where
    pre' = unpack $ config ^. path . renkonPrefix

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
