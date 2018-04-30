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
    -- * Типы псевдонимы
    ,   Tags
    ,   Files
) where


import System.Directory
import System.Posix.Files
import Data.List.Extra
import Control.Monad

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
