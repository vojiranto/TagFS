-- | Создаём индексы, для более удобного ориентирования по файлам.
module Index where

import System.Directory
import System.Posix.Files
import Data.List.Extra
import Control.Monad
import Tags
-- qualified

-- | Выясняем, какие файлы  относятся к тем или иным тегам и создаём
--   соответствующий индекс.
makeTageIndex :: IO ()
makeTageIndex = do
    aHomeDirectory <- getHomeDirectory
    let aTagPath = aHomeDirectory ++ "/.tagFS/index/tag/"
    aTags <- getTagList
    forM_ aTags $ \aTag -> do
        aFiles <- getFileList aTag
        writeFile (aTagPath ++ aTag) $ concat [aFile ++ "\n"| aFile <- aFiles]

-- | Взятие списка файлов
getAllFileList :: IO [String]
getAllFileList = do
    aHomeDirectory <- getHomeDirectory
    listDirectory $ aHomeDirectory ++ "/.tagFS/files"


-- | Выясняем, какие теги проставлены тем или иным тегам.
makeFileIndex :: IO ()
makeFileIndex = do
    aHomeDirectory <- getHomeDirectory
    aTags <- getTagList

    -- строим индекс тегов в оперативной памяти.
    aTagIdex <- forM aTags $ \aTag -> do
        aFiles <- getFileList aTag
        return $ (aTag, aFiles)

    aAllFiles <- getAllFileList
    let aTagPath = aHomeDirectory ++ "/.tagFS/index/file/"
    forM_ aAllFiles $ \aFile -> do
        let aIndex = [aTag | (aTag, aList) <- aTagIdex, aFile `elem` aList]
        writeFile (aTagPath ++ aFile ++ ".txt") $ 
            concat [aTag ++ "\n" | aTag <- aIndex]
