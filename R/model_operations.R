#' List available models
#'
#' Retrieves model names of all available PMML models on ZEMENTIS Server.
#'
#' @return A character vector that lists all available PMML models on ZEMENTIS
#'   Server.
#' @importFrom magrittr %>%
#' @export
get_models <- function() {

  url <- paste(get_zementis_base_url(), "models", sep = "/")

  response <- httr::GET(url, httr::authenticate(get_zementis_usr(),
                                                get_zementis_pwd()),
                        httr::user_agent(get_useragent()))

  if (httr::http_error(response)) {
    stop(
      sprintf(
        "Zementis Server API request failed [%s]\n%s\n%s\n%s",
        httr::status_code(response),
        httr::http_status(response)$category,
        httr::http_status(response)$reason,
        httr::http_status(response)$message
      ),
      call. = FALSE
    )
  }

  if (httr::http_type(response) != "application/json") {
    stop("Zementis Server API did not return json", .call = FALSE)
  }

  httr::content(response, as = "text") %>%
    jsonlite::fromJSON() %>%
    unlist(use.names = FALSE)
}

#' Activate existing PMML model
#'
#' Activates an existing PMML model which was deployed to ZEMENTIS Server.
#'
#' @param name The name of the model that is activated on ZEMENTIS server.
#' @return If the model name is not known to the server, an error. Otherwise a
#'  list with components:
#'  \item{model_name}{The name of the activated model}
#'  \item{is_active}{The activation status of the model}
#' @importFrom magrittr %>%
#' @export
activate_model <- function(name) {

  url <- paste(get_zementis_base_url(), "model", name, "activate",
               sep = "/")

  response <- httr::PUT(url, httr::authenticate(get_zementis_usr(),
                                                get_zementis_pwd()),
                        httr::user_agent(get_useragent()))

  if (httr::http_error(response)) {
    error_message <- sprintf(
      "Zementis Server API request failed [%s]\n%s\n%s\n%s",
      httr::status_code(response),
      httr::http_status(response)$category,
      httr::http_status(response)$reason,
      httr::http_status(response)$message
    )
    if(status_code(response) == 404) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }


    stop(error_message, call. = FALSE)
  }
  parsed <- httr::content(response, as = "text") %>%
    jsonlite::fromJSON()

  list(
    model_name = parsed[["modelName"]],
    is_active = parsed[["isActive"]]
  )
}

