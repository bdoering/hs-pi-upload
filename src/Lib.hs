{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( myApp
    ) where

import Control.Applicative ((<$>), optional)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import Text.RawString.QQ (r)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Upload
import qualified Config

-- type CfgMonad = ReaderT L.ByteString (ServerPartT IO) 
-- runCfgMonad :: CfgMonad a -> L.ByteString -> ServerPart a
-- runCfgMonad k c = runReaderT k c
    

myApp :: ServerPart Response
myApp = msum
  [ dir "echo"    $ echo
  -- , dir "query"   $ queryParams
  -- , dir "form"    $ formPage
  -- , dir "fortune" $ fortune
  , dir "static"   $ fileServing
--  , dir "upload"  $ upload
  , dir "upload-target" $ uploadTarget
  , homePage
  ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

homePage :: ServerPart Response
homePage =
    ok $ template "home page" $ do
           H.h1 "Hello!"
           H.p "Writing applications with happstack-lite is fast and simple!"
           H.p "Check out these killer apps."
           H.p $ a ! href "/echo/secret%20message"  $ "echo"
           H.p $ a ! href "/query?foo=bar" $ "query parameters"
           H.p $ a ! href "/form"          $ "form processing"
           H.p $ a ! href "/fortune"       $ "(fortune) cookies"
           H.p $ a ! href "/files"         $ "file serving"
           H.p $ a ! href "/upload"        $ "file uploads"

echo :: ServerPart Response
echo =
    path $ \(msg :: String) ->
        ok $ template "echo" $ do
          p $ "echo says: " >> toHtml msg
          p "Change the url to echo something else."

fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing [""] Config.staticDir 
            

uploadTarget :: ServerPart Response
uploadTarget =
    msum [ uploadTargetGet
         , uploadTargetPost
         ]

-- > upload :: ServerPart Response
-- > upload =
-- >        msum [ uploadForm
-- >             , handleUpload
-- >             ]
-- >     where
-- >     uploadForm :: ServerPart Response
-- >     uploadForm =
-- >         do method GET
-- >            ok $ template "upload form" $ do
-- >              form ! enctype "multipart/form-data" ! A.method "POST" ! action "/upload" $ do
-- >                input ! type_ "file" ! name "file_upload" ! size "40"
-- >                input ! type_ "submit" ! value "upload"
-- >
-- >     handleUpload :: ServerPart Response
-- >     handleUpload =
-- >         do (tmpFile, uploadName, contentType) <- lookFile "file_upload"
-- >            ok $ template "file uploaded" $ do
-- >                 p (toHtml $ "temporary file: " ++ tmpFile)
-- >                 p (toHtml $ "uploaded name:  " ++ uploadName)
-- >                 p (toHtml $ "content-type:   " ++ show contentType)
