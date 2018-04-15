module Tags (
        tags
    ,   getFileList
    ,   tagRename
    ,   forceTagRename
) where


import System.Directory
import System.Posix.Files
import Data.List.Extra hiding (find)
import Control.Monad

type Tags  = [String]
type Files = [String]

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


-- переименовывание тега
tagRename :: String -> String -> IO ()
tagRename aOldName aNaweName = do
    aTagList <- getTagList
    if aNaweName `notElem` aTagList
        then forceTagRenameInternal aTagList aOldName aNaweName
        else putStrLn $  "Тег \"" ++ aNaweName ++ "\" уже существует."


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