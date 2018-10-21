#' Download PMML model
#'
#' Download PMML source from Zementis Server.
#'
#' @param model_name Name of the PMML model to download.
#' @param annotated Logical indicating if server should return annotated PMML
#'   model source. The annotated source may contain warnings embedded in XML comments
#'   that are useful for debugging. (Default: \code{FALSE})
#' @inheritParams get_models
#' @return  A list with the following components:
#'  \itemize{
#'    \item \code{model_name} The \code{model_name} of the downloaded model
#'      including the suffix ".pmml".
#'    \item \code{model_source} An S3 object of class \code{XMLInternalDocument}
#'      created by parsing the server response using \code{XML::xmlParse()}.
#'    }
#'@seealso \code{\link{upload_model}}
#' @export
#'
#' @examples
#'  \dontrun{
#'    download_model("iris_model")
#'
#'    # Download all models and save them to disk
#'    downloads <- get_models() %>% purrr::map(download_model)
#'    file_names <- purrr::map_chr(downloads, "model_name")
#'    purrr::walk2(purrr::map(downloads, "model_source"),
#'                file_names,
#'                XML::saveXML)
#'  }
download_model <- function(model_name, annotated = FALSE, ...) {
  if (length(model_name) != 1L || typeof(model_name) != "character") {
    stop("'model_name' must be a length-one character vector")
  }

  url <- paste(get_zementis_base_url(), "model" , model_name,  "source",
               sep = "/") %>%
    httr::modify_url(query = list(annotated = annotated))
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
    if (httr::status_code(response) == c(404)) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }
  parsed <- httr::content(response, as = "text", encoding = "UTF-8") %>%
    XML::xmlParse()
  list(
    model_name = paste(model_name, ".pmml", sep = ""),
    model_source = parsed
  )
}
