{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Renkon.Util where

import Turtle.Format


ln :: (a ~ b) => Format a b
ln = "\n"
