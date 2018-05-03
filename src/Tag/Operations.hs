{-#Language MultiWayIf#-}
module Tag.Operations (
        makeAlias
    ,   tagMake
    ,   tagDelete
    ,   tagAddToFile
    ,   tagAddToTag
    ,   getTagList
    ,   cleanTheNames
    ,   forceTagRename
    ,   tagRename
    ,   substitute
    ,   makeFormTag
  ) where

import Tags
import System.IO
import System.Directory
import System.Posix.Files
import Control.Monad
import Data.Graph
import Data.List
import Parser

makeAlias :: String -> String -> IO ()
makeAlias aTag aAlias = do
    aTags <- getTagList
    when (aAlias `notElem` aTags) $ do
        aHomeDirectory <- getHomeDirectory
        createSymbolicLink
            (aHomeDirectory ++ "/.tagFS/tags/" ++ aTag)
            (aHomeDirectory ++ "/.tagFS/tags/" ++ aAlias)


-- создание тега
tagMake :: String -> IO ()
tagMake aTagName = do
    aTagList <- getTagList
    when (aTagName `notElem` aTagList) $ do
        aHomeDirectory <- getHomeDirectory
        createDirectory $ aHomeDirectory  ++ "/.tagFS/tags/" ++ aTagName


-- переименовывание тега
tagRename :: String -> String -> IO ()
tagRename aOldName aNaweName = do
    aTagList <- getTagList
    if aNaweName `notElem` aTagList
        then forceTagRenameInternal aTagList aOldName aNaweName
        else putStrLn $  "Тег \"" ++ aNaweName ++ "\" уже существует."


-- заменить все вхождения xs в ys.
substitute :: String -> String -> String -> String
substitute a b y = if
    | length a > length y       -> y
    | take (length a) y == a    -> b ++ substitute a b (drop (length a) y)
    | x:xs <- y                 -> x:substitute a b xs
    | otherwise                 -> []


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
    aFormTags <- filterM  (\a -> doesFileExist $ aPath ++ a) aTagList
    forM_ aFormTags $ \aTag -> modifyFile
        (aPath ++ aTag) (aSubstitute aOldName aNaweName)
    rename (aPath ++ aOldName)  (aPath ++ aNaweName)
    putStrLn $ "Тег \"" ++ aOldName ++ "\" переименован в \"" ++ aNaweName ++ "\"."
  where
    aSubstitute a b = substitute ('"':a ++ "\"" ) ('"':b ++ "\"" )

modifyFile :: String -> (String -> String) -> IO ()
modifyFile aPath aF = do
    aFileString <- withFile aPath ReadMode hGetLine
    writeFile aPath (aF aFileString)

-- взятие списка тегов
getTagList :: IO [String]
getTagList = do
    aHomeDirectory <- getHomeDirectory
    listDirectory $ aHomeDirectory ++ "/.tagFS/tags"


checkOfCycle :: String -> [String] -> IO [String]
checkOfCycle aMetaTag aTags = do
    aHomeDirectory <- getHomeDirectory
    aTagList <- getTagList
    aGraph   <- forM aTagList $ \aTag -> do
        aKeys <- listDirectory $ aHomeDirectory ++ "/.tagFS/tags/" ++ aTag
        return (aTag, aTag, aKeys)
    return $ concat $ do
         CyclicSCC a <- stronglyConnComp $ (aMetaTag, aMetaTag, aTags):aGraph
         return a


-- удаление тега из системы
tagDelete :: String -> IO ()
tagDelete aTagName = do
    aHomeDirectory <- getHomeDirectory
    aTags <- getTagList
    forM_ aTags $ \aTag -> do
        let aTagPath = aHomeDirectory ++ "/.tagFS/tags/" ++ aTag ++ "/" ++ aTagName
        aOk1 <- doesFileExist $ aHomeDirectory ++ "/.tagFS/tags/" ++ aTag
        unless aOk1 $ do
            aOk2 <- fileExist aTagPath
            when aOk2 $ removeFile aTagPath
    let aTagPath = aHomeDirectory ++ "/.tagFS/tags/" ++ aTagName
    isFile      <- doesFileExist      aTagPath
    isDirectory <- doesDirectoryExist aTagPath
    when isDirectory $ removeDirectoryRecursive aTagPath
    when isFile      $ removeLink aTagPath


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


-- проверяем, что тег А полностью входит в тег Б.
tagIsInTag :: String -> String -> IO Bool
tagIsInTag aTagA aTagB = do
    (aTags, _) <- takeTagAndFilesList aTagB
    return $ aTagA `elem` aTags



-- убираем в начале файла префикс
toNorm :: String -> String
toNorm aString = if take 10 aString == "Ссылка на "
    then drop 10 aString
    else aString


-- добавление тега к файлу
tagAddToFile :: String -> String -> IO()
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


makeFormTag :: String -> String -> IO()
makeFormTag aName aForm = do
    aHomeDirectory <- getHomeDirectory
    aOk1 <- checkOfCycle aName [aTags |Tag aTags <- lexer aForm]
    let aTagPath = aHomeDirectory ++ "/.tagFS/tags/" ++ aName
    aOk2 <- doesPathExist aTagPath
    aOk3 <- doesFileExist aTagPath
    when ((aOk3 || not aOk2) && null aOk1) $ do
        putStrLn "Тег создан."
        writeFile aTagPath aForm
    unless (null aOk1) $ putStrLn $ "Образуют цикл: " ++ intercalate ", " aOk1

tagAddToTag :: String -> String -> IO()
tagAddToTag aMetaTagName aTagName = do
    aHomeDirectory <- getHomeDirectory
    let aTagPath      = aHomeDirectory ++ "/.tagFS/tags/" ++ aTagName
        aMetaTagPath  = aHomeDirectory ++ "/.tagFS/tags/" ++ aMetaTagName
    aOk1 <- doesPathExist aTagPath
    aOk2 <- doesPathExist aMetaTagPath
    aOk3 <- checkOfCycle aMetaTagName [aTagName]
    if aOk1 && aOk2 && null aOk3 then
        createSymbolicLink aTagPath (aMetaTagPath ++ "/" ++ aTagName)
    else do
        putStrLn "Невозможно добавление метатега: "
        unless aOk1 $ putStrLn "Метатег не существует"
        unless aOk2 $ putStrLn "Тег не существует"
        unless (null aOk3) $ putStrLn $ "Образуют цикл: " ++ intercalate ", " aOk3
