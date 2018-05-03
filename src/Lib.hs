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

import Tags
import File
import Link
import Init
import Tag.Operations
import System.Environment


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
        ["s", aRequest] -> do
            putStrLn $ "Выполнение поискового запроса " ++ aRequest
            requestFind aRequest

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

        ["tag", aTagName, aFileName] -> do
            putStrLn $ "Добавление тега " ++ aTagName ++ " к файлу " ++ aFileName
            tagAddToFile aTagName aFileName

        ["tagm", aMetaTag, aTagName] -> do
            putStrLn $ "Добавление метатега " ++ aMetaTag ++ " к тегу " ++ aTagName
            tagAddToTag aMetaTag aTagName

        ["alias", aName, aAlias] -> do
            putStrLn "Создан псевдоним для тега."
            makeAlias aName aAlias

        ["ln", aName, aLink] -> do
            putStrLn "Создан файл ссылка."
            makeLink aName aLink
        _                                       -> return ()
