#' Checks for YDisk token in the Global Environment
#'
#' @description `set_YD_oath` checks if OAuth token for Yandex Disk is set in the Global Environment
#'
#' @details
#' If the OAuth token for Yandex Disk API is set in Global Environment as YDisk,
#' or anything else there starts with OAuth y0_, the function returns it as the OAth token
#'
#' @return a string or an error
#' @export
#' @examples
#' set_YD_oath()

set_YD_oath <- function(){
  YD_oath <- Sys.getenv("YDisk")
  if(YD_oath != ""){
    print("There is YDisk variable is found in Global Environment. It will be used as OAuth token for Yandex Disk")
    return(YD_oath)
  } else {
    genv <- Sys.getenv()[which(grepl("^OAuth y0_", Sys.getenv()))]
    YD_oath <- as.character(genv)
    YD_cand <- names(genv)
    if(YD_oath != ""){
      cat(paste0("There is no YDisk variable in Global Environment, but ",
                   YD_cand, " variable is foudn -- it starts with OAuth y0_... and is probably ",
                   "the Yandex Disk OAuth token. You can run Sys.getenv(",YD_cand,") and check if this is true."))
      return(YD_oath)
    } else {
      cat(paste0('There is no YDisk variable or anything similar in Global Environment. ',
                   'If you are going to use this package, Run file.edit("~/.Renviron") and',
                   ' set OAuth token there in a new line like YDisk=OAuth y0_...)'))
      break()
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
#' @return a tibble
#' @export
#' @examples
#' get_YD_folders(YD_oath = set_YD_oath())
#' get_YD_folders(path = "disk:/my_project_X/", YD_oath = set_YD_oath())
get_YD_folders <- function(path = "disk:/", YD_oath){
  if(grepl("^disk:\\/", path)){
    folders <- paste0("https://cloud-api.yandex.net/v1/disk/resources",
                      "?path=", path ,"&limit=100") |>
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
      print("looks like there are no folders at the provided path")
    }
    return(folders)
  } else {
    print("the path should start with disk:/")
    break()
  }
}
