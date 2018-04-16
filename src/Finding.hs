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
    showLinks  :: a -> IO ()


-- | Найти все файлы из списка и поместить их в папку "Поиск".
instance Finding (IO [String]) where
    writeLinks :: IO [String] -> IO ()
    writeLinks aFiles = do
        aPureFiles <- aFiles

        aHomeDirectory <- getHomeDirectory
        clearDirectory $ aHomeDirectory ++ "/Поиск"

        -- размещаем в ней ссылки на файлы соответствующего тега
        putStrLn $ "Число найденых объектов " ++ show (length aPureFiles) ++ "."
        forM_ aPureFiles $ \aFile -> createSymbolicLink
            (aHomeDirectory ++ "/.tagFS/files/" ++ aFile)
            (aHomeDirectory ++ "/Поиск/" ++ aFile)

    showLinks :: IO [String] -> IO ()
    showLinks aFiles = do
        aPureFiles <- aFiles
        putStrLn $ "Число найденых объектов " ++ show (length aPureFiles)
        forM_ aPureFiles putStrLn


-- | Найти все файлы под тегом и поместить их в папку "Поиск".
instance Finding String where
    writeLinks :: String -> IO ()
    writeLinks = writeLinks . getFileList

    showLinks :: String -> IO ()
    showLinks = showLinks . getFileList


-- | Теоретико множественные операции над списками файлов получаемых по тегам.
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
