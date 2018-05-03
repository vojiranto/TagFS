{-#Language MultiWayIf#-}
module File (addFile) where

import System.Directory
import System.Posix.Files


-- | Добавление файла в системуц учёта под именем
addFile :: String -> String -> IO ()
addFile aFilePath aFileName = do
    aHomeDirectory <- getHomeDirectory
    if  | head aFilePath == '/' -> createSymbolicLink aFilePath
            (aHomeDirectory ++ "/.tagFS/files/" ++ aFileName)
        | take 2 aFilePath == "./" -> do
            aCurrentDirectory <- getCurrentDirectory
            createSymbolicLink
                (aCurrentDirectory ++ "/" ++ drop 2 aFilePath)
                (aHomeDirectory ++ "/.tagFS/files/" ++ aFileName)
        | otherwise -> do
            aCurrentDirectory <- getCurrentDirectory
            createSymbolicLink
                (aCurrentDirectory ++ "/" ++ aFilePath)
                (aHomeDirectory ++ "/.tagFS/files/" ++ aFileName)
