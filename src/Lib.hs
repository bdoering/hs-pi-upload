{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( myApp
    ) where

import           Happstack.Lite
import           System.FilePath ((</>))
import           Control.Monad.Trans (liftIO)
import qualified Config
import           Upload

myApp :: ServerPart Response
myApp = msum
        [ dir "static" fileServing
        , dir "upload-target" uploadTarget
        , homePage
        ]

homePage :: ServerPart Response
homePage = do
  let p = Config.staticDir </> "html" </> "upload.html"
  serveFile (asContentType "text/html") p

fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing [""] Config.staticDir

uploadTarget :: ServerPart Response
uploadTarget =
    msum [ uploadTargetGet
         , uploadTargetPost
         ]

