#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

get_useragent <- function() {
  ua <- "Request created by the zementisr package; https://github.com/alex23lemm/zementisr"
  ua
}

get_zementis_base_url <- function() {
  val <- Sys.getenv("ZEMENTIS_base_url")
  if (identical(val, "")) {
    stop("`ZEMENTIS_base_url` env var has not been set.")
  }
  val
}

get_zementis_usr <- function() {
  val <- Sys.getenv("ZEMENTIS_usr")
  if (identical(val, "")) {
    stop("`ZEMENTIS_usr` env var has not been set.")
  }
  val
}

get_zementis_pwd <- function() {
  val <- Sys.getenv("ZEMENTIS_pwd")
  if (identical(val, "")) {
    stop("`ZEMENTIS_pwd` env var has not been set.")
  }
  val
}
