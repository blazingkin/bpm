module Main where

import qualified Heartbeat
import BPM
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> do showHelp
        [arg] ->
            when arg == "init"
                Heartbeat.init
        _     -> error "command not implemented"

