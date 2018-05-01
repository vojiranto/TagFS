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
        tags
    ,   getFileList
    ,   getTagList
    ,   tagRename
    ,   tagDelete
    ,   tagMake
    ,   makeAlias
    ,   tagAddToFile
    ,   tagAddToTag
    ,   forceTagRename
    ,   cleanTheNames
    ,   requestFind
    -- * Типы псевдонимы
    ,   Tags
    ,   Files
    ,   Finding (..)
    ,   TagSet (..)
) where


import System.Directory
import System.Posix.Files
import Data.List.Extra
import Control.Monad
import Parser

type Tags  = [String]
type Files = [String]

tags :: IO ()
tags = do
    aTags <- getTagList
    forM_ aTags putStrLn

makeAlias :: String -> String -> IO ()
makeAlias aTag aAlias = do
    aTags <- getTagList
    when (aAlias `notElem` aTags) $ do
        aHomeDirectory <- getHomeDirectory
        createSymbolicLink
            (aHomeDirectory ++ "/.tagFS/tags/" ++ aTag)
            (aHomeDirectory ++ "/.tagFS/tags/" ++ aAlias)


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


-- переименовывание тега
tagRename :: String -> String -> IO ()
tagRename aOldName aNaweName = do
    aTagList <- getTagList
    if aNaweName `notElem` aTagList
        then forceTagRenameInternal aTagList aOldName aNaweName
        else putStrLn $  "Тег \"" ++ aNaweName ++ "\" уже существует."


-- создание тега
tagMake :: String -> IO ()
tagMake aTagName = do
    aTagList <- getTagList
    when (aTagName `notElem` aTagList) $ do
        aHomeDirectory <- getHomeDirectory
        createDirectory $ aHomeDirectory  ++ "/.tagFS/tags/" ++ aTagName


-- добавление тега к файлу
tagAddToFile :: String -> String -> IO ()
tagAddToFile aTagName aFileName = do
    aHomeDirectory <- getHomeDirectory
    let aFilePath = aHomeDirectory ++ "/.tagFS/files/" ++ aFileName
        aTagPath  = aHomeDirectory ++ "/.tagFS/tags/" ++ aTagName
    aOk1 <- doesPathExist aTagPath
    aOk2 <- doesPathExist aFilePath
    if aOk1 && aOk2 then
        createSymbolicLink aFilePath (aTagPath ++ "/" ++ aFileName)
    else do
        unless aOk1 $ putStrLn "Тег не существует"
        unless aOk2 $ putStrLn "Файл не существует"


tagAddToTag :: String -> String -> IO ()
tagAddToTag aMetaTagName aTagName = do
    aHomeDirectory <- getHomeDirectory
    let aTagPath      = aHomeDirectory ++ "/.tagFS/tags/" ++ aTagName
        aMetaTagPath  = aHomeDirectory ++ "/.tagFS/tags/" ++ aMetaTagName
    aOk1 <- doesPathExist aTagPath
    aOk2 <- doesPathExist aMetaTagPath
    if aOk1 && aOk2 then
        createSymbolicLink aTagPath (aMetaTagName ++ "/" ++ aTagName)
    else do
        unless aOk1 $ putStrLn "Метатег не существует"
        unless aOk2 $ putStrLn "Тег не существует"


-- удаление тега из системы
tagDelete :: String -> IO ()
tagDelete aTagName = do
    aHomeDirectory <- getHomeDirectory
    aTags <- getTagList
    forM_ aTags $ \aTag -> do
        let aTagPath = aHomeDirectory ++ "/.tagFS/tags/" ++ aTag ++ "/" ++ aTagName
        aOk <- fileExist aTagPath
        when aOk $ removeFile aTagPath
    removeDirectoryRecursive $ aHomeDirectory ++ "/.tagFS/tags/" ++ aTagName



-- переименовать тег, без проверки на наличие совпадений.
forceTagRename :: String -> String -> IO ()
forceTagRename aOldName aNaweName = do
    aTagList <- getTagList
    forceTagRenameInternal aTagList aOldName aNaweName


-- внутренняя часть переименования тега
forceTagRenameInternal :: [String] -> String -> String -> IO ()
forceTagRenameInternal aTagList aOldName aNaweName = do
    aTags    <- filterM (tagIsInTag aOldName) aTagList
    aHomeDirectory <- getHomeDirectory
    let aPath = aHomeDirectory ++ "/.tagFS/tags/"
    forM_ aTags $ \aTag -> do
        removeLink $ aPath ++ aTag ++ "/" ++ aOldName
        createSymbolicLink
            (aPath ++ aNaweName)
            (aPath ++ aTag ++ "/" ++ aNaweName)
    rename (aPath ++ aOldName)  (aPath ++ aNaweName)
    putStrLn $ "Тег \"" ++ aOldName ++ "\" переименован в \"" ++ aNaweName ++ "\"."


-- проверяем, что тег А полностью входит в тег Б.
tagIsInTag :: String -> String -> IO Bool
tagIsInTag aTagA aTagB = do
    (aTags, _) <- takeTagAndFilesList aTagB
    return $ aTagA `elem` aTags


cleanTheNames :: IO ()
cleanTheNames = do
    aTags           <- getTagList
    aHomeDirectory  <- getHomeDirectory
    forM_ aTags $ \aTag -> do
        let aTagPath = aHomeDirectory ++ "/.tagFS/tags/" ++ aTag
        aFileNames <- listDirectory aTagPath
        forM_ aFileNames $ \aFileName -> rename
            (aTagPath ++ "/" ++ aFileName)
            (aTagPath ++ "/" ++ toNorm aFileName)

-- убираем в начале файла префикс
toNorm :: String -> String
toNorm aString = if take 10 aString == "Ссылка на "
    then drop 10 aString
    else aString


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
        return (aTagsA `union` aTagsB, aFilesA `union` aFilesB)

    a !- b = do
        (aTagsA, aFilesA) <- getFileList a
        (aTagsB, aFilesB) <- getFileList b
        return (aTagsA \\ aTagsB, aFilesA \\ aFilesB)

    a !* b = do
        (aTagsA, aFilesA) <- getFileList a
        (aTagsB, aFilesB) <- getFileList b
        return (aTagsA `intersect` aTagsB, aFilesA `intersect` aFilesB)


instance TagSet (IO (Tags, Files)) String where
    a !+ b = do
        (aTagsA, aFilesA) <- a
        (aTagsB, aFilesB) <- getFileList b
        return (aTagsA `union` aTagsB, aFilesA `union` aFilesB)

    a !- b = do
        (aTagsA, aFilesA) <- a
        (aTagsB, aFilesB) <- getFileList b
        return (aTagsA \\ aTagsB, aFilesA \\ aFilesB)

    a !* b = do
        (aTagsA, aFilesA) <- a
        (aTagsB, aFilesB) <- getFileList b
        return (aTagsA `intersect` aTagsB, aFilesA `intersect` aFilesB)


instance TagSet String (IO (Tags, Files)) where
    a !+ b = do
        (aTagsA, aFilesA) <- getFileList a
        (aTagsB, aFilesB) <- b
        return (aTagsA `union` aTagsB, aFilesA `union` aFilesB)

    a !- b = do
        (aTagsA, aFilesA) <- getFileList a
        (aTagsB, aFilesB) <- b
        return (aTagsA \\ aTagsB, aFilesA \\ aFilesB)

    a !* b = do
        (aTagsA, aFilesA) <- getFileList a
        (aTagsB, aFilesB) <- b
        return (aTagsA `intersect` aTagsB, aFilesA `intersect` aFilesB)


instance TagSet (IO (Tags, Files)) (IO (Tags, Files)) where
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
