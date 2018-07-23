module Main where

import Remote.Package
import BPM
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> do showHelp
        [arg] -> getPackage arg
        _     -> error "command not implemented"

