#' List available models
#'
#' Retrieves names of all available PMML models on Zementis Server.
#'
#' @param ... Additional arguments passed on to the underlying HTTP method.
#'   This might be necessary if you need to set some curl options explicitly
#'   via \code{\link[httr]{config}}.
#' @return A character vector that lists all available PMML models on Zementis
#'   Server.
#' @export
get_models <- function(a, ...) {

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
  httr::content(response, as = "text") %>%
    jsonlite::fromJSON() %>%
    purrr:::flatten_chr()
}


#' Upload new PMML model
#'
#' Upload new PMML source or serialized model file to Zementis Server.
#'
#' @param file Path to a file. The PMML model file can end in \code{.xml},
#' \code{.pmml} or even be \code{.zip} or \code{.gzip} compressed.
#' @param applyCleanser Logical indicating if the server should perfom cleansing
#'  on the PMML file. (Default: TRUE)
#' @return  If a model with the same name already existis on the server or if is
#'  not valid PMML, an error.
#' Otherwise if is a valid PMML model, a list with the following components:
#'  \item{model_name}{The name of the uploaded model}
#'  \item{is_active}{A logical indicating the activation status of the model
#'    which is TRUE after the initial model upload.}
#' @export
#'
#' @examples
#'  \dontrun{
#'    #Build a simple lm model
#'    iris_lm <- lm(Sepal.Length ~ ., data=iris)
#'    # Convert to pmml and save to disk
#'    iris_pmml <- pmml(iris_lm, model.name = "iris_model")
#'    saveXML(iris_pmml, "iris_pmml.xml")
#'
#'    # Upload model to server
#'    upload_model("iris_pmml.xml")
#'  }
upload_model <- function(file, applyCleanser = TRUE) {

  if (!file.exists(file)) {
    stop("Please provide a valid path to the model file.", .call = FALSE)
  }
  applyCleanser <- ifelse(applyCleanser, "true", "false")
  url <- paste(get_zementis_base_url(), "model" , sep = "/") %>%
    httr::modify_url(query = list(applyCleanser = applyCleanser))
  my_file <- httr::upload_file(file)

  response <- httr::POST(url, httr::authenticate(get_zementis_usr(),
                                                 get_zementis_pwd()),
                         httr::user_agent(get_useragent()),
                         body = list(file = my_file))

  if (httr::status_code(response) != 201) {
    error_message <- sprintf(
      "Zementis Server API request failed [%s]\n%s\n%s\n%s",
      httr::status_code(response),
      httr::http_status(response)$category,
      httr::http_status(response)$reason,
      httr::http_status(response)$message
    )
    if (httr::status_code(response) %in% c(400, 409)) {
      error_message <- paste(error_message,
                             content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }
  if (httr::http_type(response) != "application/json") {
    stop("Zementis Server API did not return json", .call = FALSE)
  }
  parsed <- httr::content(response, as = "text") %>%
    jsonlite::fromJSON()

  list(
    model_name = parsed[["modelName"]],
    is_active = parsed[["isActive"]]
  )
}


#' Delete PMML model
#'
#' Remove PMML model from server and list remaining models.
#'
#' @param model_name Name of the model to be deleted.
#' @return If deletion is successful, a vector of names of models remaining
#' deployed on the server with \code{model_name} missing in it. Otherwise, an
#' error.
#' @export
#'
#' @examples
#' \dontrun{
#'  delete("iris_model")
#'
#'  #Delete all models
#'  get_models() %>% purrr:::map(delete_model)
#'  }
delete_model <- function(model_name) {

  stopifnot(is.character(model_name))

  url <- paste(get_zementis_base_url(), "model",
               gsub(" ", "%20", model_name),
               sep = "/")

  response <- httr::DELETE(url, httr::authenticate(get_zementis_usr(),
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
    if(httr::status_code(response) == 404) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }

  if (httr::http_type(response) != "application/json") {
    stop("Zementis Server API did not return json", .call = FALSE)
  }
  httr::content(response, as = "text") %>%
    jsonlite::fromJSON() %>%
    purrr:::flatten_chr()
}


#' Activate existing PMML model
#'
#' Activates an existing PMML model which was deployed to Zementis Server.
#'
#' @param model_name The name of the model that is activated on Zementis server.
#' @return If the model name is not known to the server, an error. Otherwise a
#'  list with components:
#'  \itemize{
#'    \item \code{model_name} The \code{model_name} of the activated model
#'    \item \code{is_active} A logical indicating the activation status of the model
#'  }
#' @export
#'
#' @examples
#' \dontrun{
#'   activate_model("iris_model")
#'
#'   #Activate all models on the server
#'   get_models() %>% purrr::map_df(activate_model)
#' }
activate_model <- function(model_name) {

  stopifnot(is.character(model_name))

  url <- paste(get_zementis_base_url(), "model",
               gsub(" ", "%20", model_name),
               "activate", sep = "/")

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
    if(httr::status_code(response) == 404) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }

  if (httr::http_type(response) != "application/json") {
    stop("Zementis Server API did not return json", .call = FALSE)
  }
  parsed <- httr::content(response, as = "text") %>%
    jsonlite::fromJSON()

  list(
    model_name = parsed[["modelName"]],
    is_active = parsed[["isActive"]]
  )
}

#' Deactivate existing PMML model
#'
#' Dectivates an existing PMML model which was deployed to Zementis Server.
#'
#' @param model_name The name of the model that is deactivated on Zementis server.
#' @return If the model name is not known to the server, an error. Otherwise a
#'  list with components:
#'  \itemize{
#'    \item \code{model_name} The \code{model_name} of the deactivated model
#'    \item \code{is_active} A logical indicating the activation status of the model
#'    }
#' @export
#'
#' @examples
#' \dontrun{
#'   deactivate_model("iris_model")
#'
#'   #Deactivate all models on the server
#'   get_models() %>% purrr::map_df(deactivate_model)
#' }
deactivate_model <- function(model_name) {

  stopifnot(is.character(model_name))

  url <- paste(get_zementis_base_url(), "model",
               gsub(" ", "%20", model_name),
               "deactivate", sep = "/")

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
    if(httr::status_code(response) == 404) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }

  if (httr::http_type(response) != "application/json") {
    stop("Zementis Server API did not return json", .call = FALSE)
  }
  parsed <- httr::content(response, as = "text") %>%
    jsonlite::fromJSON()

  list(
    model_name = parsed[["modelName"]],
    is_active = parsed[["isActive"]]
  )
}
