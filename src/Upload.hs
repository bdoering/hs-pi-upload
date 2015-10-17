{-# LANGUAGE OverloadedStrings #-}

module Upload
       ( uploadTargetGet
       , uploadTargetPost
       , extractParams
       ) where

import           Config
import           Control.Monad
import           Control.Monad.Trans         (liftIO)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.List                   (intercalate, sort)
import           Data.List.Split             (splitOn)
import qualified Data.Text.Lazy              as TL
import           Happstack.Lite
import           Happstack.Server.Monads     (askRq)
import           Happstack.Server.Types      (takeRequestBody, unBody)
import           System.Directory
import           System.FilePath             (takeExtension, (</>))
import qualified System.FilePath.Glob        as G
import           System.IO                   (IOMode (WriteMode), withFile)
import           System.Process              (callCommand)
import           Text.Blaze.Html5            (Html, a, form, input, label, p,
                                              toHtml, (!))
import           Text.Blaze.Html5.Attributes (action, enctype, href, name, size,
                                              type_, value)

data Chunk = Chunk { identifier :: String
                   , number     :: Int
                   , nChunks    :: Int
                   , filename   :: String }
             deriving (Show)

mkFilename :: Chunk -> FilePath
mkFilename c = Config.tmpUploadDir </> identifier c ++ ".part" ++ show (number c)

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

uploadTargetGet :: ServerPart Response
uploadTargetGet = do
  method GET
  chunk <- extractParams
  liftIO $ print chunk
  let fn = mkFilename chunk
  fileExists <- liftIO $ doesFileExist fn
  if fileExists then setResponseCode 200 else setResponseCode 204
  return $ toResponse ()

uploadTargetPost :: ServerPart Response
uploadTargetPost = do
  method POST
  chunk <- extractParams
  let fn = mkFilename chunk
  fileData <- getBody
  liftIO $ BL.writeFile fn fileData
  -- Check if all chunks have been loaded, possibly combine them to
  -- final file
  chunkFilenames <- liftIO $ G.globDir1 (G.compile (identifier chunk ++ ".part*")) Config.tmpUploadDir
  when (length chunkFilenames == nChunks chunk) $ do
                     liftIO $ combineChunks chunk chunkFilenames
                     liftIO $ mapM_ removeFile chunkFilenames
                     -- Send a Telegram message to a group chat
                     let msg = "Received file: *" ++ filename chunk ++ "*"
                     liftIO . callCommand $ "curl --data chat_id=" ++
                       Config.botChatId ++ " --data-urlencode \"text=" ++
                       msg ++ "\" --data parse_mode=Markdown https://api.telegram.org/bot" ++
                       Config.botApiKey ++ "/sendMessage"

  return $ toResponse ()

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
      outFn = Config.uploadedDir </> sanitizeFilename (filename c)
  withFile outFn WriteMode $ \h ->
    forM_ sortedFiles $ \f -> BL.readFile f >>= BL.hPut h


