module Main where

import           Happstack.Lite

import           Lib

main :: IO ()
main = serve Nothing myApp
