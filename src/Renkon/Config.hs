{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Renkon.Config where

import Data.Text as Text
import Control.Lens
import System.Environment
import System.FilePath
import System.Directory
import GHC.Generics

data PathConfig = PathConfig
  { _renkonRoot :: FilePath
  , _renkonBin :: FilePath
  , _renkonPrefix :: Text
  }
  deriving (Show, Generic)

makeLenses ''PathConfig

data Config = Config
  { _path :: PathConfig }
  deriving (Show, Generic)

makeLenses ''Config

setupPathConfig :: IO PathConfig
setupPathConfig = do
  root' <- (</> ".renkon") <$> getHomeDirectory
  return $ PathConfig root' (root' </> "bin/") "renkon-"

boot :: IO Config
boot = do
  config <- Config <$> setupPathConfig
  exportPath config
  return config

exportPath :: Config -> IO ()
exportPath config = do
  path' <- getEnv "PATH"
  let bin' = config ^. path . renkonBin
  setEnv "PATH" $ bin' ++ ":" ++ path'
