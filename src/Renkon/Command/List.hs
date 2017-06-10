{-# LANGUAGE OverloadedStrings #-}

module Renkon.Command.List where

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens.Operators
import System.FilePath
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

  onDirAbsence bin' $ do
    guard False

  let pre' = config ^. path . renkonPrefix
  path' <- getSearchPath
  gens' <- findFilesWith (return . isRenconGenerator) path' ""
  withColor Green $ do
    mapM_ (fprint ("  " % string % ln)) gens'

isRenconGenerator :: FilePath -> Bool
isRenconGenerator x = True

onDirAbsence :: (MonadIO m) => FilePath -> m () -> m ()
onDirAbsence dir action = do
  exists <- liftIO $ doesDirectoryExist dir
  when (not exists) $ do
    action
