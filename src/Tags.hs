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
    if isADirectory then do
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


-- извлекаем список файлов относящихся к тегу
getFileList :: String -> IO (Tags, Files)
getFileList aTag = do
    aHomeDirectory <- getHomeDirectory
    aMeta <- doesFileExist $ aHomeDirectory ++ "/.tagFS/tags/" ++ aTag ++ "/meta"
    (aTags, aFiles) <- aFilter =<< aGetFilesList [] [] [] [aTag]
    if aMeta then return (aTags, [])
    else return (aTags, aFiles)
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


-- | Теоретико множественные операции над списками файлов получаемых по тегам.
infixl 7 !*
infixl 6 !-, !+

(!+), (!-), (!*) :: IO (Tags, Files) -> IO (Tags, Files) -> IO (Tags, Files)
a !+ b = do
    (aTagsA, aFilesA) <- a
    (aTagsB, aFilesB) <- b
    return (aTagsA `union` aTagsB, aFilesA `union` aFilesB)

a !- b = do
    (aTagsA, aFilesA) <- a
    (aTagsB, aFilesB) <- b
    return (aTagsA \\ aTagsB, aFilesA \\ aFilesB)

a !* b = do
    (aTagsA, aFilesA) <- a
    (aTagsB, aFilesB) <- b
    return (aTagsA `intersect` aTagsB, aFilesA `intersect` aFilesB)

-- преобразование дерева в поисковый запрос
findTags :: FuncTree -> IO (Tags, Files)
findTags = \case
    ListNode a   -> getFileList a
    AndNode  a b -> findTags a !* findTags b
    OrNode   a b -> findTags a !+ findTags b
    NotNode  a b -> findTags a !- findTags b

requestFind :: String -> IO ()
requestFind = writeLinks . findTags . toFuncTree . toBracketTree . lexer
