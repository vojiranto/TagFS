--  TODO: Написать редми.
--      - Описать, какие предположения о структуре файлов делает система.
--  TODO: Операции над файлами и тегами:
--      - построение графа зависимостей между тегами, проверка его на ацекличность.
--      - переименования файла;
--      - переименование тега (учесть, новое имя в формульном теге)
--      - определение списка тегов, по которому может быть найден файл;
--      - нахождение связанных тегов.
--  TODO: Найти решение для рекурсивных зависимостей в тегах.
--  TODO: Метаинформация и описание:
--      - для тега;
--      - для файла.
--  TODO: Поиск по метаинформации.
--  TODO: Автоприсваиваемые теги: "Изображения", "Документы", "Видео"...
--  TODO: Автодополнение файлов из папок.
--  TODO: Экспорт файловой системы в теговую.
--  TODO: ГУЙ.
--  TODO: ГиперСтраница

module Lib (someFunc) where

import  Tags
import  File
import  Link
import  Init
import  Data.List
import  Tag.Operations
import  System.Environment
import  Control.Monad

someFunc :: IO ()
someFunc = do
    args <- getArgs
    case args of
        ["init"] -> do
            putStrLn "Создание базовая структуры системы."
            initFS
        ["clean"] -> do
            putStrLn "Чистка имён ссылок."
            cleanTheNames
        ["add", aFilePath, aFileName] -> do
            putStrLn "Добавление файла в систему"
            addFile aFilePath aFileName
        ["f", aRequest] -> do
            putStrLn $ "Выполнение поискового запроса " ++ aRequest
            requestFind aRequest

        "s":aRequest -> do
            let aForm   = intercalate "*" $ toTag <$> aRequest
                toTag a = "\"" ++ a ++"\""
            putStrLn $ "Выполнение поискового запроса " ++ aForm
            requestFind aForm

        ["mk", aName] -> do
            putStrLn $ "Создание тега " ++ aName
            tagMake aName
        ["mk", aName, aForm] -> do
            putStrLn $ "Создание тега " ++ aName
            makeFormTag aName aForm

        ["rn", aOldName, aNewName] -> do
            putStrLn $ "Переименование тега " ++ aOldName ++ " в " ++ aNewName
            tagRename aOldName aNewName

        ["rm", aName] -> do
            putStrLn $ "Удаление тега " ++ aName
            tagDelete aName

        "tag":aFileName:aTagNames -> do
            putStrLn $ "Добавление тегов к файлу " ++ aFileName
            forM_ aTagNames $ \aTagName -> tagAddToFile aTagName aFileName

        "tagm":aTagName:aMetaTags -> do
            putStrLn $ "Добавление метатегов к тегу " ++ aTagName
            forM_ aMetaTags $ \aMetaTag -> tagAddToTag aTagName aMetaTag

        "alias":aName:aAliases -> do
            putStrLn "Создание псевдонимов для тега."
            forM_ aAliases $ makeAlias aName

        ["ln", aName, aLink] -> do
            putStrLn "Создан файл ссылка."
            makeLink aName aLink
        _                                       -> return ()
