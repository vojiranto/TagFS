# TagFS

Система для организации данных на компьютере и поиска по ним. Для этого используется система тегов и метатегов. При этом все данные разбиваются на множества, а поиск сводится к действиям над этими множествами.

Формат команд:
```
tfs initFS
tfs addFile aFilePath aFileName
tfs find '"tag1"+ ("tag2" - "tag3")*"tag4"'
tfs tagMake 'aName'
tfs tagRename 'aOldName' 'aNewName'
tfs tagDelete 'aName'
tfs tagAddToFile 'aTagName' 'aFileName'
tfs tagAddToTag 'aMetaTagName' 'aTagName'
tfs makeAlias 'aName' 'aAlias'
tfs makeLink 'aName' 'aLink'
```
