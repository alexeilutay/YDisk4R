utils::globalVariables(c("type"))

#' Checks for YDisk token in the Global Environment
#'
#' @description `set_YD_oauth` tries to find Yandex Disk OAuth token in Global Environment
#'
#' @details
#' First, the function seeks the YDisk in Global Environment. If YDisk is absent in GE,
#' it checks if there is a variable starting with OAuth y0_ and takes it as YD OAuth.
#'
#' @return a string or NULL with warning
#' @export
#' @examples
#'\dontrun{
#' set_YD_oauth()
#' }
set_YD_oauth <- function(){
  ydoauth <- Sys.getenv("YDisk")
  if(ydoauth != ""){
    print("YDisk is found in Global Environment and will be used as OAuth token for Yandex Disk")
    return(ydoauth)
  } else {
    genv <- Sys.getenv()[which(grepl("^OAuth y0_", Sys.getenv()))]
    if(!purrr::is_empty(genv)){
      ydoauth <- as.character(genv)
      YD_cand <- names(genv)
      cat(paste0("YDisk is not set in Global Environment, but ",
                   YD_cand, " variable starts with OAuth y0_... and is probably ",
                   "how you store Yandex Disk OAuth token. You can run Sys.getenv(",YD_cand,") and check if this is true."))
      return(ydoauth)
    } else {
      cat(paste0('We found nothing resembling OAuth token in Global Environment. ',
                   'To use this package further, you may set OAuth manually as variable, ',
                   'or Run file.edit("~/.Renviron") and save OAuth token there as a separate line YDisk=OAuth y0_...'))
      return(NULL)
    }
  }
}


#' Folders present at the provided YDisk path
#'
#' @description `get_YD_folders` returns a list of dirs at the path provided
#' @import httr
#' @import purrr
#' @import dplyr
#'
#' @details
#' The path preset by default is disk:/ (root folder of your YDisk),
#' set own path starting with disk:/. If there are no folders at the indicated path,
#' the function return an empty tibble.
#'
#' @param path a path at YDisk, it should start with disk:/
#' @param token an OAuth token for YDisk
#' @param limit how many folders to return, by default 100
#' @return a tibble
#' @export
#' @examples
#'\dontrun{
#' yd_oauth <- set_YD_oauth()
#' get_YD_folders(token = yd_oauth)
#' get_YD_folders(path = "disk:/Загрузки/", limit = 5, token = set_YD_oauth())
#' }
get_YD_folders <- function(path = "disk:/", limit = 100, token){
  if(grepl("^disk:\\/", path)){
    folders <- paste0("https://cloud-api.yandex.net/v1/disk/resources",
                      "?path=", path ,"&limit=", limit) |>
      httr::GET(httr::add_headers(Accept = "application/json",
                                  Authorization = token)) |>
      httr::content() |>
      purrr::pluck("_embedded") |>
      purrr::pluck("items")

    folders <- Filter(function(x) x$type == "dir", folders) |>
          purrr::map_df(~.x %>% purrr::flatten())

    if(nrow(folders)==0){
      print("looks like at the path provided there are no folders")
    }
    return(folders)
  } else {
    print("the path should start with disk:/")
    return(NULL)
  }
}

#' Files present at the provided YDisk path
#'
#' @description `get_YD_files` returns a list of files at the path provided
#' @import httr
#' @import purrr
#' @import dplyr
#' @import stringr
#' @details
#' The path preset by default is disk:/ (root folder of your YDisk),
#' set own path starting with disk:/. If there are no folders at the indicated path,
#' the function return an empty tibble.
#'
#' @param path a path at YDisk, it should start with disk:/
#' @param token an OAuth token for YDisk
#' @param limit how many files to return, by default 25
#' @return a tibble
#' @export
#' @examples
#'\dontrun{
#' yd_oauth <- set_YD_oauth()
#' get_YD_files(token = set_YD_oauth())
#' get_YD_files(path = "disk:/Загрузки/", limit = 100, token = yd_oauth)
#' }
get_YD_files <- function(path = "disk:/", limit = 100, token){
  if(grepl("^disk:\\/", path)){
    xfiles <- paste0("https://cloud-api.yandex.net/v1/disk/resources",
                      "?path=", path ,"&limit=", limit) |>
      httr::GET(httr::add_headers(Accept = "application/json",
                                  Authorization = token)) |>
      httr::content() |>
      purrr::pluck("_embedded") |>
      purrr::pluck("items")

    xfiles <-  Filter(function(x) x$type == "file", xfiles) |>
      purrr::map_df(~.x %>%
              purrr::modify_at("sizes",
                            \(x) purrr::map(x, ~paste(.x$name, "::", .x$url))) %>%
              purrr::modify_at("sizes",
                            \(x) paste(x, collapse = " | ")) %>%
                purrr::flatten()
              )

    if(nrow(xfiles)==0){
      print("looks like there are no files at the provided path")
    }
    return(xfiles)
  } else {
    print("the path should start with disk:/")
    return(NULL)
  }
}

#' File uploader for Yandex Disk API
#'
#' @description `upload_file_2YD` uploads the files to the folder of YD
#' @import httr
#' @import purrr
#' @import stringr
#' @details
#' The function uploads the file from hard disk to the specified folder (path) of YD.
#'
#' @param path path to folder where to upload, by default is disk:/ (should start with disk:/).
#' @param yd_fname the filename to use for the uploaded file at Yandex disk. If not set, it is the same as the disk_fname.
#' @param disk_fname the filename (to be uploaded) on your hard disk
#' @param token an OAuth token for YDisk
#' @param overwrite if the file should be overwritten (by defaul = FALSE)
#'
#' @export
#'
#' @examples
#'\dontrun{
#'yd_oauth <- set_YD_oauth()
#'upload_file_2YD(path = "disk:/Загрузки/", disk_fname = "../../bookmarks.html",
#'                overwrite = TRUE, token = yd_oauth)
#'}
upload_file_2YD <- function(path = "disk:/", yd_fname = NULL, disk_fname,
                            overwrite=FALSE, token){
  if(is.null(yd_fname)){
    yd_fname <- stringr::str_extract(disk_fname, "[^\\/]+$")
  }
  path <- gsub("[\\/]+", "/", path)
  xref <- httr::GET(paste0("https://cloud-api.yandex.net/v1/disk/resources/upload",
                     "?path=", paste0(path, yd_fname),
                     "&overwrite=", overwrite,
                     "&fields=name,_embedded.items.path"),
              httr::add_headers(Accept = "application/json",
                          Authorization = token)) |>
    httr::content() |>
    purrr::pluck("href")
  print("Uploading is being started. Check the status: 201 would mean the file's been uploaded")
  httr::PUT(url = xref,
            body = upload_file(disk_fname))
}


#' Get a list of Public Files
#'
#' @description `my_public_files` uploads the files to the folder of YD
#' @import httr
#' @import purrr
#' @details
#' The function returns a dataframe with all the details for your public files
#'
#' @param limit how many files to return, by default 100
#' @param token an OAuth token for YDisk
#' @return a dataframe
#' @export
#'
#' @examples
#' my_public_files(limit = 1000, token = set_YD_oauth())
my_public_files <- function(limit = 100, token){
  xfiles <- paste0("https://cloud-api.yandex.net/v1/disk/resources/public?limit=", limit) |>
    httr::GET(httr::add_headers(Accept = "application/json",
                                Authorization = token)) |>
    httr::content() |>
    purrr::pluck("items") |>
    purrr::map_df(~.x %>%
                    purrr::modify_at("sizes",
                                     \(x) purrr::map(x, ~paste(.x$name, "::", .x$url))) %>%
                    purrr::modify_at("sizes",
                                     \(x) paste(x, collapse = " | ")) %>%
                    purrr::flatten()
    )
  return(xfiles)
}

#' Download a file from Yandex.Disk matching a pattern
#'
#' @description
#' ydisk_download_file() retrieves a list of files from a specified directory on Yandex.Disk,
#' filters them by a regular expression pattern, and downloads the single matching file
#' to a local folder. After download, it verifies the file size to ensure integrity.
#'
#' @param yd_path Character string. The path to the directory on Yandex.Disk.
#'   The path is automatically converted to UTF-8 and URL-encoded.
#' @param yd_filename_pattern Character string. A regular expression pattern used to
#'   filter filenames. The matching is performed with dplyr::str_detect().
#' @param dest_folder Character string. The local directory path where the downloaded
#'   file will be saved. The directory should exist; the function does not create it.
#' @param yd_token Character string. YDisk4R::set_YD_oauth()
#'
#' @importFrom utils URLencode download.file
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @return Invisibly returns `NULL`. Called for its side effect: downloading a file
#'   and printing messages about the outcome.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming yd_token is already set in your environment
#' ydisk_download_file(
#'   yd_path = "/Documents",
#'   yd_filename_pattern = "2025.*\\.xlsx",
#'   dest_folder = "~/Downloads/yd_downloads"
#'   yd_token = YDisk4R::set_YD_oauth()
#' )
#' }
#'
#' @seealso [YDisk4R::get_YD_files()] for obtaining file listings,
#'   [YDisk4R::set_YD_oauth()] for authentication.
ydisk_download_file <- function(yd_path, yd_filename_pattern, dest_folder, yd_token){
  files <- get_YD_files(token = yd_token, path = URLencode(enc2utf8(yd_path)))
  if(nrow(files)>0){
    files <- files |> filter(str_detect(name, yd_filename_pattern))
    if(nrow(files)==1){
      jfilename <- file.path(dest_folder, files$name[1])
      if(!file.exists(jfilename)){
        download.file(files$file[1], destfile = jfilename, quiet = T)
        if(between(file.size(jfilename)/files$size[1], 0.98, 1.02)){
          cat(paste0('the file ', jfilename, ' has been saved'))
        }
      } else {
        cat(paste0('the file ', jfilename, ' already exists'))
      }
    }
    if(nrow(files)==0){
      cat(paste0('No file matches with the provided pattern {', yd_filename_pattern, '}. Try another pattern, check the path.'))
    }
    if(nrow(files)>1){
      cat(paste0('There are >1 files matching the provided pattern {', yd_filename_pattern, '}. Try different pattern.'))
    }
  }
}
