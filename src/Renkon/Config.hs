{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Renkon.Config where

import Prelude hiding (FilePath)
import Control.Lens
import GHC.Generics
import Turtle

data PathConfig = PathConfig
  { _renkonRoot :: FilePath }
  deriving (Show, Generic)

makeLenses ''PathConfig

data Config = Config
  { _path :: PathConfig }
  deriving (Show, Generic)

makeLenses ''Config

setupPathConfig :: IO PathConfig
setupPathConfig = do
  home' <- home
  return $ PathConfig (home' </> ".renkon")

boot :: IO Config
boot = Config <$> setupPathConfig
