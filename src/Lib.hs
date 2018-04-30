--  TODO: Написать редми.
--      - Описать, какие предположения о структуре файлов делает система.
--  TODO: Операции над файлами и тегами:
--      - переименования файла;
--      - определение списка тегов, по которому может быть найден файл;
--      - нахождение связанных тегов.
--  TODO: Формульные теги (именование для запроса).
--  TODO: Метаинформация для тега, классы тегов.
--  TODO: Метоинформация:
--      - для тега: описание примечание, класс;
--      - для файла: описание примечание.
--  TODO: Автоприсваиваемые теги: "Изображения", "Документы", "Видео"...
--  TODO: ГУИ.

module Lib (
    -- * Операции для поиска и вывода результатов
        Finding (..)
    ,   TagSet (..)
    -- * Взятие данных из системы
    ,   tags
    ,   getFileList
    -- * Создание файла-ссылки на файл в сети.
    ,   makeLink
    -- * Операции над файлами
    ,   addFile
    -- * Операции над тегами
    ,   tagMake
    ,   tagRename
    ,   tagDelete
    ,   tagAddToFile
    ,   tagAddToTag
    ,   forceTagRename
    ,   makeAlias
    ,   cleanTheNames
    -- * Создать индексы
    ,   makeTageIndex
    ,   makeFileIndex
    -- * Инициация вспомогательных структур
    ,   initFS
    ,   someFunc
) where

import Tags
import Finding
import File
import Link
import Index
import Init

someFunc :: IO ()
someFunc = putStrLn "someFunc"
