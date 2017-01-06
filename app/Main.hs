module Main where

import           Happstack.Lite

import           Data.Maybe
import           Lib
import qualified Config

main :: IO ()
main = do
  let serverConfig = Just defaultServerConfig { port = Config.port }
  serve serverConfig myApp
