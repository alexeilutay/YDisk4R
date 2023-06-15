utils::globalVariables(c("type"))

#' Checks for YDisk token in the Global Environment
#'
#' @description `set_YD_oath` tries to find Yandex Disk OAuth token in Global Environment
#'
#' @details
#' First, the function seeks the YDisk in Global Environment. If YDisk is absent in GE,
#' it checks if there is a variable starting with OAuth y0_ and takes it as YD OAuth.
#'
#' @return a string or NULL with warning
#' @export
#' @examples
#'\dontrun{
#' set_YD_oath()
#' }
set_YD_oath <- function(){
  ydoath <- Sys.getenv("YDisk")
  if(ydoath != ""){
    print("YDisk is found in Global Environment and will be used as OAuth token for Yandex Disk")
    return(ydoath)
  } else {
    genv <- Sys.getenv()[which(grepl("^OAuth y0_", Sys.getenv()))]
    if(!purrr::is_empty(genv)){
      ydoath <- as.character(genv)
      YD_cand <- names(genv)
      cat(paste0("YDisk is not set in Global Environment, but ",
                   YD_cand, " variable starts with OAuth y0_... and is probably ",
                   "how you store Yandex Disk OAuth token. You can run Sys.getenv(",YD_cand,") and check if this is true."))
      return(ydoath)
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
#' @param YD_oath an OAuth token for YDisk
#' @param limit how many folders to return, by default 25
#' @return a tibble
#' @export
#' @examples
#'\dontrun{
#' get_YD_folders(YD_oath = set_YD_oath())
#' get_YD_folders(path = "disk:/Загрузки/", limit = 5, YD_oath = set_YD_oath())
#' }
get_YD_folders <- function(path = "disk:/", limit = 25, YD_oath){
  if(grepl("^disk:\\/", path)){
    folders <- paste0("https://cloud-api.yandex.net/v1/disk/resources",
                      "?path=", path ,"&limit=", limit) |>
      httr::GET(httr::add_headers(Accept = "application/json",
                                  Authorization = YD_oath)) |>
      httr::content() |>
      purrr::pluck("_embedded") |>
      purrr::pluck("items")  |>
      purrr::map_df(
        ~dplyr::tibble(
          name = .x |> purrr::pluck("name"),
          type = .x |> purrr::pluck("type"),
          path = .x |> purrr::pluck("path")
          )
        ) |>
      dplyr::filter(type=="dir")

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
#'
#' @details
#' The path preset by default is disk:/ (root folder of your YDisk),
#' set own path starting with disk:/. If there are no folders at the indicated path,
#' the function return an empty tibble.
#'
#' @param path a path at YDisk, it should start with disk:/
#' @param YD_oath an OAuth token for YDisk
#' @param limit how many files to return, by default 25
#' @return a tibble
#' @export
#' @examples
#'\dontrun{
#' get_YD_files(YD_oath = set_YD_oath())
#' get_YD_files(path = "disk:/Загрузки/", limit = 100, YD_oath = set_YD_oath())
#' }
get_YD_files <- function(path = "disk:/", limit = 25, YD_oath){
  if(grepl("^disk:\\/", path)){
    files <- paste0("https://cloud-api.yandex.net/v1/disk/resources",
                      "?path=", path ,"&limit=", limit) |>
      httr::GET(httr::add_headers(Accept = "application/json",
                                  Authorization = YD_oath)) |>
      httr::content() |>
      purrr::pluck("_embedded") |>
      purrr::pluck("items")  |>
      purrr::map_df(
        ~dplyr::tibble(
          name = .x |> purrr::pluck("name"),
          type = .x |> purrr::pluck("type"),
          yd_size = .x |> purrr::pluck("size"),
          yd_created = .x |> purrr::pluck("created") |>
            as.character() |> stringr::str_extract("\\d{4}-\\d{2}-\\d{2}"),
          file = .x |> purrr::pluck("file")
          )
        )

    if(nrow(files)==0){
      print("looks like there are no files at the provided path")
    }
    return(files)
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
#' @param YD_oath an OAuth token for YDisk
#' @param overwrite if the file should be overwritten (by defaul = FALSE)
#'
#' @export
#'
#' @examples
#'\dontrun{
#'upload_file_2YD(path = "disk:/Загрузки/", disk_fname = "../../bookmarks.html",
#'                overwrite = TRUE, YD_oath = set_YD_oath())
#'}
upload_file_2YD <- function(path = "disk:/", yd_fname = NULL, disk_fname,
                            overwrite=FALSE, YD_oath){
  if(is.null(yd_fname)){ yd_fname <- stringr::str_extract(disk_fname, "[^\\/]+$") }
  xref <- httr::GET(paste0("https://cloud-api.yandex.net/v1/disk/resources/upload",
                     "?path=", paste0(path, yd_fname),
                     "&overwrite=", overwrite,
                     "&fields=name,_embedded.items.path"),
              httr::add_headers(Accept = "application/json",
                          Authorization = YD_oath)) |>
    httr::content() |>
    purrr::pluck("href")
  httr::PUT(url = xref,
            body = upload_file(disk_fname))
  print("the status 201 means the file has been uploaded")
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
#' @param YD_oath an OAuth token for YDisk
#' @return a dataframe
#' @export
#'
#' @examples
#' my_public_files(limit = 1000, YD_oath = set_YD_oath())
my_public_files <- function(limit = 100, YD_oath){
  xfiles <- paste0("https://cloud-api.yandex.net/v1/disk/resources/public?limit=", limit) |>
    httr::GET(httr::add_headers(Accept = "application/json", Authorization = YD_oath)) |>
    httr::content() |>
    purrr::pluck("items")

  check <- function(x,name){
    # to remove the elements named `sizes` (ref. https://stackoverflow.com/a/54777781)
    m = names(x) %in% name
    x = if(any(m)) x[!m] else x
    if(is.list(x)) sapply(x,check,name)
    else x
  }

  my_public_files_df <- xfiles |>
    purrr::map_df(~check(.x, "sizes") |> purrr::flatten())
  return(my_public_files_df)
}

