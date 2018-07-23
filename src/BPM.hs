module BPM
    ( showHelp
    ) where

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
help = ("BPM Version " ++ version ++ "\nManages packages for the blz open-source programming language\n\nUsage: BPM [-h] package\n\nCommands:\n  -h    Shows help information")