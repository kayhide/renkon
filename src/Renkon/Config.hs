{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Renkon.Config where

import Data.Maybe
import Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
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


-- * Booting

boot :: IO Config
boot = do
  config <- Config <$> setupPathConfig
  exportPath config
  return config


-- * Utilities relating to cofig contents

takeGeneratorName :: Config -> FilePath -> String
takeGeneratorName config = stripPrefix' . takeBaseName
  where
    stripPrefix' x = fromMaybe x $ List.stripPrefix pre' x
    pre' = Text.unpack $ config ^. path . renkonPrefix


-- * Internal functions

setupPathConfig :: IO PathConfig
setupPathConfig = do
  root' <- (</> ".renkon") <$> getHomeDirectory
  return $ PathConfig root' (root' </> "bin/") "renkon-"

exportPath :: Config -> IO ()
exportPath config = do
  path' <- getEnv "PATH"
  let bin' = config ^. path . renkonBin
  setEnv "PATH" $ bin' ++ ":" ++ path'
