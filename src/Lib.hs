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

module Lib (
    -- * Взятие данных из системы
        getFileList
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
    ,   substitute
    ,   requestFind
) where

import Tags
import File
import Link
import Index
import Init
import Tag.Operations


someFunc :: IO ()
someFunc = do
    aRequest <- getLine
    requestFind aRequest
