# TagFS

Система для организации данных на компьютере и поиска по ним. Для этого используется система тегов и метатегов. При этом все данные разбиваются на множества, а поиск сводится к действиям над этими множествами.

Формат команд:
```
tfs init
tfs add  'aFilePath' 'aFileName'
tfs s  '"tag1"+ ("tag2" - "tag3")*"tag4"'
tfs mk aName
tfs rn aoldname anewname
tfs rm aAName
tfs tag TagName FileNma
tfs tagm metatag tagname
tfs alias aname alias
tfs ln name link
```
