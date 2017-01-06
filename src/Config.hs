module Config
    ( tmpUploadDir
    , uploadedDir
    , staticDir
    , botApiKey
    , botChatId
    , port) where

import           System.FilePath ((</>))

tmpUploadDir, uploadedDir, staticDir :: FilePath
tmpUploadDir = "/mnt/passport/tmp/tmp-uploads"          -- Where chunks are stored
uploadedDir = "/mnt/passport/hs-pi-upload_uploadedDir"  -- Where final files are stored
staticDir = "/mnt/passport/src/hs-pi-upload/static/"    -- Where static HTML resources are stored

-- See https://www.domoticz.com/wiki/Telegram_Bot#Test_Your_New_Bot_and_Get_Your_chat_id
-- about how to set up a bot and get the Api/Chat IDs
botName = "courbepiUploadBot"
botApiKey = "133529879:AAHLLqk8tbI-h8a16I3qdZ5hJAN4-dNHC84"
botChatId = "82415746"

-- Server configuration
port :: Int
port = 8001
