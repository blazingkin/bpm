{-# LANGUAGE QuasiQuotes #-}
module BPM
    ( showHelp
    ) where

import Data.String.Quote

version :: String
version = (show major) ++ "." ++ (show minor) ++ "." ++ (show patch) ++ message
    where
        major = 0
        minor = 1
        patch = 0
        message = "-prerelease"

showHelp :: IO ()
showHelp = putStrLn help

help :: String
help = "BPM Version " ++ version ++ [s|
Manages packages for the blz open-source programming language

Usage: BPM [-h] package

Commands:
  -h    Shows help information|]