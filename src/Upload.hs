{-# LANGUAGE OverloadedStrings #-}

module Upload
    ( uploadTargetGet
    , uploadTargetPost
    ) where

import Control.Applicative ((<$>), optional)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Happstack.Server.Monads (askRq)
import Happstack.Server.Types (unBody, takeRequestBody)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import Text.RawString.QQ (r)
import System.FilePath ((</>), takeExtension)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified System.FilePath.Glob as G

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Chunk = Chunk { identifier :: String
                   , number :: Int 
                   , nChunks :: Int
                   , filename :: String } 
             deriving (Show)

uploadDir = "/tmp" </> "uploads"
uploadedDir = "/tmp" </> "uploaded"

mkFilename :: Chunk -> FilePath
mkFilename c = uploadDir </> identifier c ++ ".part" ++ show (number c)

extractParams = do
  i <- lookText "resumableIdentifier"
  n <- lookText "resumableChunkNumber"
  c <- lookText "resumableTotalChunks"
  f <- lookText "resumableFilename"
  return $ Chunk
             (TL.unpack i)
             (read . TL.unpack $ n)
             (read . TL.unpack $ c)
             (TL.unpack f)
                

-- Chunk <$> (fmap TL.unpack $ lookText "resumableIdentifier")
--                 <*> (read (lookText "resumableChunkNumber") :: Int)
--                 <*> (read T(lookText "resumableTotalChunks") :: Int)
--                 <*> (fmap TL.unpack $ lookText "resumableFilename")

    
uploadTargetGet :: ServerPart Response
uploadTargetGet = do
  method GET
  chunk <- extractParams
  liftIO $ print chunk
  setResponseCode 204 
  return $ toResponse ("Test"::String)


uploadTargetPost :: ServerPart Response
uploadTargetPost = do
  body <- getBody
  liftIO $ print body
  setResponseCode 204
  return $ toResponse ("TEST"::String)

-- put this function in a library somewhere
getBody :: ServerPart BL.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return "" 
