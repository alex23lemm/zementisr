#' Apply PMML model to multiple input records
#'
#' \code{apply_model_batch()} returns the prediction for multiple input records
#' that are sent to Zementis Server. The values returned depend on the type of prediction model
#' being executed on the server.
#'
#' @param data Either a data frame or a path to a file that contain multiple data
#'   records that are sent to Zementis Server for prediction. Files must be
#'   be \code{.csv} or \code{.json} files. Alternatively, \code{.csv} and \code{.json}
#'   files can also be sent in compressed format (\code{.zip} or \code{.gzip}).
#' @param model_name The name of the deployed PMML model that gets predictions
#'  on the new data records contained in \code{data}.
#' @inheritParams get_models
#' @return A list with the following components:
#' \itemize{
#'   \item \code{model} A vector containg the \code{model_name}
#'   \item \code{outputs} A data frame containing the prediction results for \code{data}
#' }
#'
#'  For regression models \code{outputs} will include a 1-column data frame with
#'  the predicted values.
#'
#'  For binary classification models \code{outputs} will include a 3-column
#'  data frame that includes the probability of class 0, the probability of
#'  class 1 and the classification class label result based on a 50\% threshold.
#' @seealso \code{\link{upload_model}}, \code{\link{apply_model}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Predict the entire iris data set
#' apply_model_batch(iris, "iris_model")
#'
#' # Predict the entire iris data set previously saved to a .json file
#' jsonlite::write_json(iris, "iris.json")
#' apply_model_batch("iris.json", "iris_model",)
#'
#' # Predict the entire iris data set previously saved to a .csv file
#' write.csv(iris, "iris.csv", row.names = FALSE)
#' apply_model_batch("iris.csv","iris_model")
#' }
apply_model_batch <- function(data, model_name, ...) {

  if(class(data) != "data.frame" && (!is.character(data) || !file.exists(data))) {
    stop("Please either provide a data frame or a path to a file.")
  }

  if(class(data) == "data.frame") {
    file <- tempfile(fileext = ".json")
    on.exit(unlink(file))
    jsonlite::write_json(data, file)
  } else {
    file <- data
  }

  url <- paste(get_zementis_base_url(), "apply" , model_name,  sep = "/")

  my_file <- httr::upload_file(file)
  response <- httr::POST(url, httr::authenticate(get_zementis_usr(),
                                                 get_zementis_pwd()),
                         httr::accept_json(),
                         httr::user_agent(get_useragent()),
                         body = list(file = my_file),
                         ...)

  if (httr::http_error(response)) {
    error_message <- sprintf(
      "Zementis Server API request failed [%s]\n%s\n%s\n%s",
      httr::status_code(response),
      httr::http_status(response)$category,
      httr::http_status(response)$reason,
      httr::http_status(response)$message
    )
    if(httr::status_code(response) %in% c(400, 404)) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }
  if (response$headers$`content-type` == "application/zip") {
    # Save compressed response to temp file
    f_zipped <- tempfile(fileext = ".zip")
    on.exit(unlink(f_zipped), add = TRUE)
    writeBin(httr::content(response, as = "raw"), f_zipped)
    # Unzip temp file
    f_unzipped <- unzip(f_zipped)
    on.exit(unlink(f_unzipped), add = TRUE)
    # Parse temp file
    parsed <- jsonlite::fromJSON(f_unzipped)
  } else {
    parsed <- httr::content(response, as = "text") %>%
      jsonlite::fromJSON()
  }
  parsed
}
