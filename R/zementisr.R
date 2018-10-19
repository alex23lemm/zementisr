#' @section Authentication:
#' Zementis Server's REST API uses HTTP Basic Authentication. For each request the client needs to provide
#' username and password. The zementisr package requires that you store your secrets and the base URL of
#' your Zementis Server as environment variables in the \code{.Renviron} file in your home directory.
#'
#' Please, make sure to set the environment variables below in your \code{.Renviron} file before
#' using functions from the zementisr package. You can easily edit \code{.Renviron} using \code{usethis::edit_r_environ()}.
#'
#' \env{ZEMENTIS_base_url = "[address]:[port]"}\cr
#' \env{ZEMENTIS_usr = "[your_username]"}\cr
#' \env{ZEMENTIS_pwd = "[your_password]"}\cr
#' @keywords internal
"_PACKAGE"
