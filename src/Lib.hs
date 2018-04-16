--  TODO: Написать редми.
--      - Описать, какие предположения о структуре файлов делает система.
--  TODO: Операции над файлами и тегами:
--      - добавления/удаления тега в систему;
--      - добавления тега файлу;
--      - переименования файла;
--      - определение списка тегов, по которому может быть найден файл;
--      - метатеги(способ сохранять частые запросы);
--      - нахождение связанных тегов.
--  TODO: Отход от плоской модели хранения файлов, тегирование директорий.
--  TODO: Автоприсваиваемые теги: "Изображения", "Документы", "Видео"...
--  TODO: Метаинформация: описание и примечание для файла.
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
    -- * Переименование тега
    ,   tagRename
    ,   forceTagRename
    -- * Создать индексы
    ,   makeTageIndex
    ,   makeFileIndex
    ,   someFunc
) where

import Tags
import Finding
import Link
import Index

someFunc :: IO ()
someFunc = putStrLn "someFunc"
