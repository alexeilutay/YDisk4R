# YDisk4R
Набор R функций для работы с Yandex Disk.

Перед началом работы рекомендуется:
- получить OAuth token https://yandex.ru/dev/id/doc/ru/register-client
- открыть file.edit("~/.Renviron") 
- сохранить токен в виде строки вида YDisk=OAuth y0_*...здесь какие-то токен...*

### Функции

set_YD_oath() - ищет значение YDisk в Global Environment или другое, начинающееся с OAuth y0_... Найденное значение использует в качестве токена для обращения к Yandex Disk.

get_YD_folders(path = "disk:/Загрузки/", YD_oath = set_YD_oath()) - возвращает список папок по указанному пути. По умолчанию, path - корневая папка Yandex Disk.

get_YD_files(path = "disk:/Загрузки/", YD_oath = set_YD_oath(), limit = 5)  - возвращает список файлов по указанному пути. По умолчанию, path - корневая папка Yandex Disk, а limit = 1000.

### Установить пакет

devtools::install_github("alexeilutay/YDisk4R")
