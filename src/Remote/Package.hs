module Remote.Package where

getPackage :: String -> IO ()
getPackage [] = error "Package name not recognized"
getPackage name = putStrLn "Not implemented"