#' List available PMML models
#'
#' Retrieves names of all available PMML models on Zementis Server.
#'
#' @param ... Additional arguments passed on to the underlying HTTP method.
#'   This might be necessary if you need to set some curl options explicitly
#'   via \code{\link[httr]{config}}.
#' @return A character vector that lists all available PMML models on Zementis
#'   Server.
#' @seealso \code{\link{upload_model}}
#' @export
get_models <- function(...) {

  url <- paste(get_zementis_base_url(), "models", sep = "/")

  response <- httr::GET(url, httr::authenticate(get_zementis_usr(),
                                                get_zementis_pwd()),
                        httr::user_agent(get_useragent()),
                        ...)

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
  httr::content(response, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    purrr::flatten_chr()
}

#' Get PMML model properties
#'
#' Get PMML model name, description, input and output field properties.
#'
#' @param model_name Name of the PMML model whose properties are requested.
#' @inheritParams  get_models
#' @return A list with the following components:
#' \itemize{
#'   \item \code{modelName} A length one character vector containing the \code{model_name}
#'   \item \code{description} A length one character vector with the description of the model
#'   \item \code{isActive} A length one character vector with the activation status of the model
#'   \item \code{inputFields} A data frame with the model input field properties (name, type, usage)
#'   \item \code{outputFields} A data frame with the model output field properties (name, type, usage)
#' }
#' @seealso \code{\link{upload_model}}
#' @export
#'
#' @examples
#' \dontrun{
#'   get_model_properties("iris_model")
#' }
get_model_properties <- function(model_name, ...) {

  url <- paste(get_zementis_base_url(), "model", model_name, sep = "/")

  response <- httr::GET(url, httr::authenticate(get_zementis_usr(),
                                                get_zementis_pwd()),
                        httr::user_agent(get_useragent()),
                        ...)

  if (httr::http_error(response)) {
    error_message <- sprintf(
      "Zementis Server API request failed [%s]\n%s\n%s\n%s",
      httr::status_code(response),
      httr::http_status(response)$category,
      httr::http_status(response)$reason,
      httr::http_status(response)$message
    )
    if (httr::status_code(response) == 404) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }

  if (httr::http_type(response) != "application/json") {
    stop("Zementis Server API did not return json", .call = FALSE)
  }
  httr::content(response, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
}


#' Delete PMML model
#'
#' Remove PMML model from server and list remaining models.
#'
#' @param model_name Name of the model to be deleted.
#' @inheritParams  get_models
#' @return If deletion is successful, a vector of names of models remaining
#' deployed on the server with \code{model_name} missing in it. Otherwise, an
#' error.
#' @seealso \code{\link{upload_model}}
#' @export
#'
#' @examples
#' \dontrun{
#'  delete("iris_model")
#'
#'  #Delete all models
#'  get_models() %>% purrr:::map(delete_model)
#'  }
delete_model <- function(model_name, ...) {

  if (length(model_name) != 1L || typeof(model_name) != "character") {
    stop("'model_name' must be a length-one character vector")
  }

  url <- paste(get_zementis_base_url(), "model",
               gsub(" ", "%20", model_name),
               sep = "/")

  response <- httr::DELETE(url, httr::authenticate(get_zementis_usr(),
                                                   get_zementis_pwd()),
                        httr::user_agent(get_useragent()),
                        ...)

  if (httr::http_error(response)) {
    error_message <- sprintf(
      "Zementis Server API request failed [%s]\n%s\n%s\n%s",
      httr::status_code(response),
      httr::http_status(response)$category,
      httr::http_status(response)$reason,
      httr::http_status(response)$message
    )
    if (httr::status_code(response) == 404) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }

  if (httr::http_type(response) != "application/json") {
    stop("Zementis Server API did not return json", .call = FALSE)
  }
  httr::content(response, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    purrr::flatten_chr()
}


#' Activate existing PMML model
#'
#' Activates an existing PMML model which was deployed to Zementis Server.
#'
#' @param model_name The name of the PMML model that is activated on Zementis server.
#' @inheritParams get_models
#' @return If the model name is not known to the server, an error. Otherwise a
#'  list with components:
#'  \itemize{
#'    \item \code{model_name} The \code{model_name} of the activated model
#'    \item \code{is_active} A logical indicating the activation status of the model
#'  }
#' @seealso \code{\link{upload_model}}, \code{\link{deactivate_model}}
#' @export
#'
#' @examples
#' \dontrun{
#'   activate_model("iris_model")
#'
#'   #Activate all models on the server
#'   get_models() %>% purrr::map_df(activate_model)
#' }
activate_model <- function(model_name, ...) {

  if (length(model_name) != 1L || typeof(model_name) != "character") {
    stop("'model_name' must be a length-one character vector")
  }

  url <- paste(get_zementis_base_url(), "model",
               gsub(" ", "%20", model_name),
               "activate", sep = "/")

  response <- httr::PUT(url, httr::authenticate(get_zementis_usr(),
                                                get_zementis_pwd()),
                        httr::user_agent(get_useragent()),
                        ...)

  if (httr::http_error(response)) {
    error_message <- sprintf(
      "Zementis Server API request failed [%s]\n%s\n%s\n%s",
      httr::status_code(response),
      httr::http_status(response)$category,
      httr::http_status(response)$reason,
      httr::http_status(response)$message
    )
    if (httr::status_code(response) == 404) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }

  if (httr::http_type(response) != "application/json") {
    stop("Zementis Server API did not return json", .call = FALSE)
  }
  parsed <- httr::content(response, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()

  list(
    model_name = parsed[["modelName"]],
    is_active = parsed[["isActive"]]
  )
}

#' Deactivate existing PMML model
#'
#' Deactivates an existing PMML model which was deployed to Zementis Server.
#'
#' @param model_name The name of the PMML model that is deactivated on Zementis server.
#' @inheritParams get_models
#' @return If the model name is not known to the server, an error. Otherwise a
#'  list with components:
#'  \itemize{
#'    \item \code{model_name} The \code{model_name} of the deactivated model
#'    \item \code{is_active} A logical indicating the activation status of the model
#'    }
#' @seealso \code{\link{upload_model}}, \code{\link{activate_model}}
#' @export
#'
#' @examples
#' \dontrun{
#'   deactivate_model("iris_model")
#'
#'   #Deactivate all models on the server
#'   get_models() %>% purrr::map_df(deactivate_model)
#' }
deactivate_model <- function(model_name, ...) {

  if (length(model_name) != 1L || typeof(model_name) != "character") {
    stop("'model_name' must be a length-one character vector")
  }

  url <- paste(get_zementis_base_url(), "model",
               gsub(" ", "%20", model_name),
               "deactivate", sep = "/")

  response <- httr::PUT(url, httr::authenticate(get_zementis_usr(),
                                                get_zementis_pwd()),
                        httr::user_agent(get_useragent()),
                        ...)

  if (httr::http_error(response)) {
    error_message <- sprintf(
      "Zementis Server API request failed [%s]\n%s\n%s\n%s",
      httr::status_code(response),
      httr::http_status(response)$category,
      httr::http_status(response)$reason,
      httr::http_status(response)$message
    )
    if (httr::status_code(response) == 404) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }

  if (httr::http_type(response) != "application/json") {
    stop("Zementis Server API did not return json", .call = FALSE)
  }
  parsed <- httr::content(response, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()

  list(
    model_name = parsed[["modelName"]],
    is_active = parsed[["isActive"]]
  )
}
