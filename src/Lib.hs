{-# Language
        MultiWayIf
    ,   MultiParamTypeClasses
    ,   TypeSynonymInstances
    ,   FlexibleInstances
#-}

--  TODO: Написать редми.
--      - Описать, какие предположения о структуре файлов делает система.
--  TODO: Операции над файлами и тегами:
--      - добавления/удаления тега в систему;
--      - Добавления тега файлу;
--      - переименования файла;
--      - переименования тега;
--  TODO: Реструктуризация кода по модулям.
--  TODO: Автоприсваиваемые теги: "Изображения", "Документы", "Видео"...
--  TODO: Метаинформация: описание и примечание для файла.
--  TODO: ГУИ.

module Lib where

import System.Directory
import System.Posix.Files
import Data.List.Extra hiding (find)
import Control.Monad

type Tags  = [String]
type Files = [String]


someFunc :: IO ()
someFunc = putStrLn "someFunc"

tags :: IO ()
tags = do
    aTags <- getTagList
    forM_ aTags putStrLn

-- взятие списка тегов
getTagList :: IO [String]
getTagList = do
    aHomeDirectory <- getHomeDirectory
    listDirectory $ aHomeDirectory ++ "/.tagFS/tags"


-- извлекаем список вложенных тегов и файлов
takeTagAndFilesList :: String -> IO (Tags, Files)
takeTagAndFilesList aTag = do
    aHomeDirectory <- getHomeDirectory
    let aTagPath = aHomeDirectory ++ "/.tagFS/tags/" ++ aTag
    aContents <- listDirectory aTagPath

    -- считываем ссылку
    aFilesAndTags <- forM aContents $ \aContent -> do
        let aPath = aTagPath ++ "/" ++ aContent
        return . takeEnd 2 . splitOn "/" =<< readSymbolicLink aPath

    -- делим ссылки на те, которые ведут к тегам и файлам
    let (aTags, aFiles) = partition (\a -> head a == "tags") aFilesAndTags
    return (last <$> aTags, last <$> aFiles)


-- извлекаем список файлов относящихся к тегу
getFileList :: String -> IO Files
getFileList aTag = return . union [] =<< aGetFilesList [] [] [aTag]
  where
    aGetFilesList :: [String] -> [String] -> [String] -> IO [String]
    aGetFilesList aProcessed aFileList aForProcessing = do
        case aForProcessing of
            []      -> return aFileList
            x:xs    -> do
                (aTags, aFiles) <- takeTagAndFilesList x
                let aNewFileList = union aFiles aFileList
                    aNewForProcessing = union aTags xs \\ aNewFileList
                aGetFilesList (x:aProcessed) aNewFileList aNewForProcessing


class Find a where
    toFile :: a -> IO ()
    find   :: a -> IO ()


-- найти все файлы из списка и поместить их в папку "Поиск"
instance Find (IO [String]) where
    toFile aFiles = do
        aHomeDirectory <- getHomeDirectory
        let aFindDirectory = aHomeDirectory ++ "/Поиск"

        -- очищаем содержимое директории "Поиск"
        removeDirectoryRecursive aFindDirectory
        createDirectory aFindDirectory

        -- размещаем в ней ссылки на файлы соответствующего тега
        aPureFiles <- aFiles
        putStrLn $ "Число найденых файлов " ++ show (length aPureFiles)
        forM_ aPureFiles $ \aFile -> createSymbolicLink
            (aHomeDirectory ++ "/.tagFS/files/" ++ aFile)
            (aHomeDirectory ++ "/Поиск/" ++ aFile)

    find aFiles = do
        aPureFiles <- aFiles
        putStrLn $ "Число найденых файлов " ++ show (length aPureFiles)
        forM_ aPureFiles putStrLn

-- найти все файлы под тегом и поместить их в папку "Поиск"
instance Find String where
    toFile = toFile . getFileList

    find = find . getFileList

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
