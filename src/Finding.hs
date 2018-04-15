{-# Language
        MultiWayIf
    ,   MultiParamTypeClasses
    ,   TypeSynonymInstances
    ,   FlexibleInstances
#-}

module Finding (
        Finding (..)
    ,   TagSet (..)
) where

import System.Directory
import System.Posix.Files
import Data.List.Extra hiding (find)
import Control.Monad
import Tags


class Finding a where
    result :: a -> IO ()
    find   :: a -> IO ()


-- найти все файлы из списка и поместить их в папку "Поиск"
instance Finding (IO [String]) where
    result aFiles = do
        aHomeDirectory <- getHomeDirectory
        let aFindDirectory = aHomeDirectory ++ "/Поиск"

        -- очищаем содержимое директории "Поиск"
        removeDirectoryRecursive aFindDirectory
        createDirectory aFindDirectory

        -- размещаем в ней ссылки на файлы соответствующего тега
        aPureFiles <- aFiles
        putStrLn $ "Число найденых объектов " ++ show (length aPureFiles)
        forM_ aPureFiles $ \aFile -> createSymbolicLink
            (aHomeDirectory ++ "/.tagFS/files/" ++ aFile)
            (aHomeDirectory ++ "/Поиск/" ++ aFile)

    find aFiles = do
        aPureFiles <- aFiles
        putStrLn $ "Число найденых объектов " ++ show (length aPureFiles)
        forM_ aPureFiles putStrLn


-- найти все файлы под тегом и поместить их в папку "Поиск"
instance Finding String where
    result = result . getFileList
    find = find . getFileList


-- теоретико множественные операции
class TagSet a b where
    infixl 7 !*
    infixl 6 !-, !+

    (!+) :: a -> b -> IO [String]
    (!-) :: a -> b -> IO [String]
    (!*) :: a -> b -> IO [String]


instance TagSet String String where
    a !+ b = union      <$> getFileList a <*> getFileList b
    a !- b = (\\)       <$> getFileList a <*> getFileList b
    a !* b = intersect  <$> getFileList a <*> getFileList b


instance TagSet (IO [String]) String where
    a !+ b = union      <$> a <*> getFileList b
    a !- b = (\\)       <$> a <*> getFileList b
    a !* b = intersect  <$> a <*> getFileList b


instance TagSet String (IO [String]) where
    a !+ b = union      <$> getFileList a <*> b
    a !- b = (\\)       <$> getFileList a <*> b
    a !* b = intersect  <$> getFileList a <*> b


instance TagSet (IO [String]) (IO [String]) where
    a !+ b = union      <$> a <*> b
    a !- b = (\\)       <$> a <*> b
    a !* b = intersect  <$> a <*> b
