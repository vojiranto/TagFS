module File (addFile) where

import System.Directory
import System.Posix.Files


-- | Добавление файла в системуц учёта под именем
addFile :: String -> String -> IO ()
addFile aFilePath aFileName = do
    aHomeDirectory <- getHomeDirectory
    createSymbolicLink
        (aHomeDirectory ++ "/" ++ aFilePath)
        (aHomeDirectory ++ "/.tagFS/files/" ++ aFileName)
