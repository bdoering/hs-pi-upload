module Config
( uploadDir
, uploadedDir
, staticDir ) where

import System.FilePath ((</>))
    
uploadDir, uploadedDir, staticDir :: FilePath
uploadDir = "/tmp" </> "uploads"
uploadedDir = "/tmp" </> "uploaded"
staticDir = "/home/bd/src/hs-pi-upload/static"
