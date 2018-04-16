module Init where

import System.Directory

initFS = do
    aHomeDirectory <- getHomeDirectory
    createDirectory $ aHomeDirectory ++ "/.tagFS"
    createDirectory $ aHomeDirectory ++ "/.tagFS/files"
    createDirectory $ aHomeDirectory ++ "/.tagFS/tags"
    createDirectory $ aHomeDirectory ++ "/.tagFS/index"
    createDirectory $ aHomeDirectory ++ "/.tagFS/index/files"
    createDirectory $ aHomeDirectory ++ "/.tagFS/index/tags"
