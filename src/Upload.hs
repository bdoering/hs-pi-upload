{-# LANGUAGE OverloadedStrings #-}

module Upload
    ( uploadTargetGet
    , uploadTargetPost
    , extractParams
    ) where

import Control.Applicative ((<$>), optional)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.List (sort, intercalate)
import Data.List.Split (splitOn)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Happstack.Server.Monads (askRq)
import Happstack.Server.Types (unBody, takeRequestBody)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import Text.RawString.QQ (r)
import System.Directory
import System.FilePath ((</>), takeExtension)
import System.IO (withFile, IOMode(WriteMode))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified System.FilePath.Glob as G
    
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Config

data Chunk = Chunk { identifier :: String
                   , number :: Int 
                   , nChunks :: Int
                   , filename :: String } 
             deriving (Show)


mkFilename :: Chunk -> FilePath
mkFilename c = Config.uploadDir </> identifier c ++ ".part" ++ show (number c)

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
  let fn = mkFilename chunk
  fileExists <- liftIO $ doesFileExist fn
  if fileExists 
  then setResponseCode 200
  else setResponseCode 204
  return $ toResponse ("Done"::String)
           
uploadTargetPost :: ServerPart Response
uploadTargetPost = do
  method POST
  chunk <- extractParams
  let fn = mkFilename chunk
  fileData <- getBody
  liftIO $ BL.writeFile fn fileData

  chunkFilenames <- liftIO $ G.globDir1 (G.compile (identifier chunk ++ ".part*")) Config.uploadDir
  when (length chunkFilenames == nChunks chunk) $ do
                     liftIO $ combineChunks chunk chunkFilenames
                     liftIO $ mapM_ removeFile chunkFilenames
  -- setResponseCode 200
  return $ toResponse ("Done"::String)

         
-- put this function in a library somewhere
getBody :: ServerPart BL.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return "" 

-- Remove '..', '/' and '\' from filenames, just to make sure...
sanitizeFilename :: FilePath -> FilePath
sanitizeFilename x = foldl clean x ["..", "/", "\\"]
    where 
      clean :: String -> String -> String
      clean s c = intercalate "" . splitOn c $ s
                                                     
combineChunks :: Chunk -> [FilePath] -> IO ()
combineChunks c xs = do
  -- Sort files by chunk number
  let sortedFiles = map snd . sort $ [((read . drop 5 . takeExtension $ fn) :: Int, fn) | fn <- xs]
      outFn = (Config.uploadedDir </> (sanitizeFilename $ filename c))
  withFile outFn WriteMode $ \h -> do
                       forM_ sortedFiles $ \f -> do
                                     (BL.readFile f) >>= BL.hPut h 
                       
  -- fileContents <- mapM BL.readFile sortedFiles
  -- BL.writeFile (Config.uploadedDir </> (sanitizeFilename $ filename c)) (BL.concat fileContents)
  
