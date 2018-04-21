{-# Language
        MultiWayIf
    ,   MultiParamTypeClasses
    ,   TypeSynonymInstances
    ,   FlexibleInstances
    ,   InstanceSigs
#-}

module Finding (
        Finding (..)
    ,   TagSet (..)
) where

import System.Directory
import System.Posix.Files
import Data.List.Extra
import Control.Monad
import Tags

-- | Очищаем содержимое директории.
clearDirectory :: String -> IO ()
clearDirectory aDirectory = do
    removeDirectoryRecursive aDirectory
    createDirectory aDirectory


class Finding a where
    writeLinks :: a -> IO ()


-- | Найти все файлы из списка и поместить их в папку "Поиск".
instance Finding (IO ([String], [String])) where
    writeLinks :: IO ([String], [String]) -> IO ()
    writeLinks aLists = do
        (aTags, aFiles) <- aLists

        aHomeDirectory <- getHomeDirectory
        clearDirectory $ aHomeDirectory ++ "/Поиск"

        -- размещаем в ней ссылки на файлы соответствующего тега
        putStrLn $ "Число найденых объектов " ++ show (length aFiles) ++ "."
        putStrLn $ "Число найденых тегов " ++ show (length aTags) ++ "."

        -- создаём ссылки на файлы
        forM_ aFiles $ \aFile -> createSymbolicLink
            (aHomeDirectory ++ "/.tagFS/files/" ++ aFile)
            (aHomeDirectory ++ "/Поиск/" ++ aFile)

        -- создаём ссылки на теги
        forM_ aTags $ \aTag -> createSymbolicLink
            (aHomeDirectory ++ "/.tagFS/tags/" ++ aTag)
            (aHomeDirectory ++ "/Поиск/" ++ aTag)

-- | Найти все файлы под тегом и поместить их в папку "Поиск".
instance Finding String where
    writeLinks :: String -> IO ()
    writeLinks = writeLinks . getFileList


-- | Теоретико множественные операции над списками файлов получаемых по тегам.
class TagSet a b where
    infixl 7 !*
    infixl 6 !-, !+

    (!+) :: a -> b -> IO (Tags, Files)
    (!-) :: a -> b -> IO (Tags, Files)
    (!*) :: a -> b -> IO (Tags, Files)


instance TagSet String String where
    a !+ b = do
        (aTagsA, aFilesA) <- getFileList a
        (aTagsB, aFilesB) <- getFileList b
        return (union aTagsA aTagsB, union aFilesA aFilesB)

    a !- b = do
        (aTagsA, aFilesA) <- getFileList a
        (aTagsB, aFilesB) <- getFileList b
        return (aTagsA \\ aTagsB, aFilesA \\ aFilesB)

    a !* b = do
        (aTagsA, aFilesA) <- getFileList a
        (aTagsB, aFilesB) <- getFileList b
        return (intersect aTagsA aTagsB, intersect aFilesA aFilesB)


instance TagSet (IO (Tags, Files)) String where
    a !+ b = do
        (aTagsA, aFilesA) <- a
        (aTagsB, aFilesB) <- getFileList b
        return (union aTagsA aTagsB, union aFilesA aFilesB)

    a !- b = do
        (aTagsA, aFilesA) <- a
        (aTagsB, aFilesB) <- getFileList b
        return (aTagsA \\ aTagsB, aFilesA \\ aFilesB)

    a !* b = do
        (aTagsA, aFilesA) <- a
        (aTagsB, aFilesB) <- getFileList b
        return (intersect aTagsA aTagsB, intersect aFilesA aFilesB)


instance TagSet String (IO (Tags, Files)) where
    a !+ b = do
        (aTagsA, aFilesA) <- getFileList a
        (aTagsB, aFilesB) <- b
        return (union aTagsA aTagsB, union aFilesA aFilesB)

    a !- b = do
        (aTagsA, aFilesA) <- getFileList a
        (aTagsB, aFilesB) <- b
        return (aTagsA \\ aTagsB, aFilesA \\ aFilesB)

    a !* b = do
        (aTagsA, aFilesA) <- getFileList a
        (aTagsB, aFilesB) <- b
        return (intersect aTagsA aTagsB, intersect aFilesA aFilesB)


instance TagSet (IO (Tags, Files)) (IO (Tags, Files)) where
    a !+ b = do
        (aTagsA, aFilesA) <- a
        (aTagsB, aFilesB) <- b
        return (union aTagsA aTagsB, union aFilesA aFilesB)

    a !- b = do
        (aTagsA, aFilesA) <- a
        (aTagsB, aFilesB) <- b
        return (aTagsA \\ aTagsB, aFilesA \\ aFilesB)

    a !* b = do
        (aTagsA, aFilesA) <- a
        (aTagsB, aFilesB) <- b
        return (intersect aTagsA aTagsB, intersect aFilesA aFilesB)
