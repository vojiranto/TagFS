--  TODO: Написать редми.
--      - Описать, какие предположения о структуре файлов делает система.
--  TODO: Операции над файлами и тегами:
--      - переименования файла;
--      - переименование тега (учесть, новое имя в формульном теге)
--      - определение списка тегов, по которому может быть найден файл;
--      - нахождение связанных тегов.
--  TODO: Найти решение для рекурсивных зависимостей в тегах.
--  TODO: Метаинформация и описание:
--      - для тега;
--      - для файла.
--  TODO: Автоприсваиваемые теги: "Изображения", "Документы", "Видео"...
--  TODO: Автодополнение файлов из папок.
--  TODO: Экспорт файловой системы в теговую.
--  TODO: ГУЙ.
--  TODO: ГиперСтраница

module Lib (someFunc) where

import Tags
import File
import Link
import Index
import Init
import Tag.Operations
import System.Environment


someFunc :: IO ()
someFunc = do
    args <- getArgs
    case args of
        ["initFS"]                              -> initFS
        ["addFile", aFilePath, aFileName]       -> addFile aFilePath aFileName
        ["find", aRequest]                      -> requestFind aRequest
        ["tagMake", aName]                      -> tagMake aName
        ["tagRename", aOldName, aNewName]       -> tagRename aOldName aNewName
        ["tagDelete", aName]                    -> tagDelete aName
        ["tagAddToFile", aTagName, aFileName]   -> tagAddToFile aTagName aFileName
        ["tagAddToTag", aMetaTagName, aTagName] -> tagAddToTag aMetaTagName aTagName
        ["makeAlias", aName, aAlias]            -> makeAlias aName aAlias
        ["makeLink", aName, aLink]              -> makeLink aName aLink
        _                                       -> return ()
