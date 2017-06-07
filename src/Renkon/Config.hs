{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Renkon.Config where

import Prelude hiding (FilePath)
import qualified Data.Text as Text
import Control.Lens
import GHC.Generics
import Turtle

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
  root' <- (</> ".renkon") <$> home
  return $ PathConfig root' (root' </> "bin/") "renkon-"

boot :: IO Config
boot = do
  config <- Config <$> setupPathConfig
  exportPath config
  return config

exportPath :: Config -> IO ()
exportPath config = do
  Just path' <- need "PATH"
  let bin' = config ^. path . renkonBin
  export "PATH" $ format (fp % ":" % s) bin' path'
