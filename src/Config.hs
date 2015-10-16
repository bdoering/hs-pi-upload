module Config
    ( tmpUploadDir
    , uploadedDir
    , staticDir ) where

import           System.FilePath ((</>))

tmpUploadDir, uploadedDir, staticDir :: FilePath
tmpUploadDir = "/tmp" </> "tmp-uploads" -- Where chunks are stored
uploadedDir = "/tmp" </> "uploaded"     -- Where final files are stored
staticDir = "/home/bd/src/hs-pi-upload/static" -- Where static HTML resources are stored
