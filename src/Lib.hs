{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( myApp
    ) where

import           Happstack.Lite

import qualified Config
import           Upload

myApp :: ServerPart Response
myApp = msum
  [ dir "static" fileServing
  , dir "upload-target" uploadTarget
  ]

fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing [""] Config.staticDir


uploadTarget :: ServerPart Response
uploadTarget =
    msum [ uploadTargetGet
         , uploadTargetPost
         ]

