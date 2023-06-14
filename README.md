# YDisk4R
Набор R функций для работы с Yandex Disk.

Перед началом работы рекомендуется:
- получить OAuth token https://yandex.ru/dev/id/doc/ru/register-client
- открыть file.edit("~/.Renviron") 
- сохранить токен в виде строки вида YDisk=OAuth y0_*...здесь какие-то токен...*

### Функции

- set_YD_oath - ищет значение YDisk в Global Environment или другое, начинающееся с OAuth y0_
- get_YD_folders - возвращает список папок по указанному пути. По умолчанию, корневая папка Yandex Disk

