{-# Language
        MultiWayIf
    ,   LambdaCase
    ,   MultiParamTypeClasses
    ,   TypeSynonymInstances
    ,   FlexibleInstances
    ,   InstanceSigs
  #-}

module Tags (
    -- Функции
        getFileList
    ,   requestFind
    ,   takeTagAndFilesList
    -- * Типы псевдонимы
    ,   Tags
    ,   Files
) where


import System.Directory
import System.Posix.Files
import Data.List.Extra
import Control.Monad
import Parser

type Tags  = [String]
type Files = [String]


-- извлекаем список вложенных тегов и файлов
takeTagAndFilesList :: String -> IO (Tags, Files)
takeTagAndFilesList aTag = do
    aHomeDirectory <- getHomeDirectory
    let aTagPath = aHomeDirectory ++ "/.tagFS/tags/" ++ aTag
    isADirectory <- doesDirectoryExist aTagPath
    aExist <- doesPathExist aTagPath
    if aExist then if isADirectory
        then do
            aContents <- listDirectory aTagPath

            -- считываем ссылку
            aFilesAndTags <- forM (filter (/= "meta") aContents) $ \aContent -> do
                let aPath = aTagPath ++ "/" ++ aContent
                takeEnd 2 . splitOn "/" <$> readSymbolicLink aPath

            -- делим ссылки на те, которые ведут к тегам и файлам
            let (aTags, aFiles) = partition (\a -> head a == "tags") aFilesAndTags
            aMeta <- doesFileExist $ aHomeDirectory ++ "/.tagFS/tags/" ++ aTag ++ "/meta"
            if aMeta then return (last <$> aTags, [])
            else return (last <$> aTags, last <$> aFiles)
        else findTags =<< toFuncTree . toBracketTree . lexer <$> readFile aTagPath
    else do
        putStrLn $ "Тег " ++ aTag ++ " не существует."
        return ([],[])

-- извлекаем список файлов относящихся к тегу
getFileList :: String -> IO (Tags, Files)
getFileList aTag = do
    aHomeDirectory <- getHomeDirectory
    if aTag /= "" then do
        aMeta <- doesFileExist $ aHomeDirectory ++ "/.tagFS/tags/" ++ aTag ++ "/meta"
        (aTags, aFiles) <- aFilter =<< aGetFilesList [] [] [] [aTag]
        if aMeta then return (aTags, [])
        else return (aTags, aFiles)
    else do
        aFiles <- getFileListOfEmptyTag
        return ([], aFiles)
  where
    aGetFilesList :: Tags -> Tags -> Files -> Tags -> IO (Tags, Files)
    aGetFilesList aProcessed aTagList aFileList aForProcessing =
        case aForProcessing of
            []      -> return (aTagList, aFileList)
            x:xs    -> do
                (aTags, aFiles) <- takeTagAndFilesList x
                let aNewFileList = aFiles `union` aFileList
                    aNewTagList  = aTags  `union` aTagList
                    aNewForProcessing = union aTags xs \\ aNewFileList
                aGetFilesList (x:aProcessed) aNewTagList aNewFileList aNewForProcessing

    aFilter :: (Tags, Files) -> IO (Tags, Files)
    aFilter (a, b) = return ([] `union` a, [] `union` b)


getFileListOfEmptyTag :: IO Files
getFileListOfEmptyTag = do
    aHomeDirectory <- getHomeDirectory
    aFiles     <- listDirectory $ aHomeDirectory ++ "/.tagFS/files"
    aTags      <- listDirectory $ aHomeDirectory ++ "/.tagFS/tags"
    aFileLists <- forM aTags $ \aTag -> do
        let aTagPath = aHomeDirectory ++ "/.tagFS/tags/" ++ aTag
        aOk <- doesDirectoryExist aTagPath
        if aOk then listDirectory aTagPath else return []
    return $ aFiles \\ foldl union [] aFileLists


-- | Очищаем содержимое директории.
clearDirectory :: String -> IO ()
clearDirectory aDirectory = do
    removeDirectoryRecursive aDirectory
    createDirectory aDirectory


-- | Найти все файлы из списка и поместить их в папку "Поиск".
writeLinks :: IO ([String], [String]) -> IO ()
writeLinks aLists = do
    (aTags, aFiles) <- aLists
    aHomeDirectory <- getHomeDirectory
    clearDirectory $ aHomeDirectory ++ "/Поиск"

    -- создаём ссылки на файлы
    aMakeLinks "files" aFiles
    aMakeLinks "tags"  aTags
  where
    aMakeLinks :: String -> [String] -> IO ()
    aMakeLinks aStr aTags = forM_ aTags $ \aTag -> do
        aHomeDirectory <- getHomeDirectory
        createSymbolicLink
            (aHomeDirectory ++ "/.tagFS/"++ aStr ++ "/" ++ aTag)
            (aHomeDirectory ++ "/Поиск/" ++ aTag)

-- преобразование дерева в поисковый запрос
findTags :: FuncTree -> IO (Tags, Files)
findTags = \case
    ListNode a   -> getFileList a
    AndNode  a b -> aOperation intersect    a b
    OrNode   a b -> aOperation union        a b
    NotNode  a b -> aOperation (\\)         a b
  where
    aOperation :: ([String] -> [String] -> [String]) -> FuncTree -> FuncTree -> IO (Tags, Files)
    aOperation aF a b = do
        (aTagsA, aFilesA) <- findTags a
        (aTagsB, aFilesB) <- findTags b
        return (aTagsA `aF` aTagsB, aFilesA `aF` aFilesB)


requestFind :: String -> IO ()
requestFind = writeLinks . findTags . toFuncTree . toBracketTree . lexer
