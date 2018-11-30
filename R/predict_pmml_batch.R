#' Get predictions for multiple input records from PMML model
#'
#' \code{predict_pmml_batch()} returns the predictions for multiple input records
#' that are sent to Zementis Server. The values returned depend on the type of prediction model
#' being executed on the server.
#'
#' When calling \code{predict_pmml_batch()} data is sent to Zementis Server using octet streams.
#' That means batch data is sent in stream mode and processing/scoring starts when the first chunk
#' of streams hits the server. By default, the server will process records in a batch size of
#' 5000 records per thread and there will be a maximum of 2*n threads to process the entire batch
#' where n is the number of available cores on the machine.
#'
#' Using the two function arguments \code{max_threads} and \code{max_records_per_thread} you can
#' modify the compute resources on the server for your data processing needs. \code{max_threads} lets you reserve
#' additional threads for your request (CPU resources). \code{max_records_per_thread} allows you to modify
#' the number of records processed by a single thread (memory resources).
#' @param data Either a data frame or a path to a file that contain multiple data
#'   records that are sent to Zementis Server for prediction. Files must
#'   be \code{.csv} or \code{.json} files. Alternatively, \code{.csv} and \code{.json}
#'   files can also be sent in compressed format (\code{.zip} or \code{.gzip}). For
#'   compressed files you need to set the \code{path} argument.
#' @param model_name The name of the deployed PMML model that gets predictions
#'  on the new data records contained in \code{data}.
#' @param path Path to a file to which the response from Zementis Server is written to. Only mandatory
#'  if compressed input files (\code{.zip}) are passed to \code{data}.
#' @param max_threads Maximum number of concurrent threads to process the data that is sent.
#'   Default value is twice the number of processor cores.
#' @param max_records_per_thread Maximum number of records processed by a single thread. Default value is 5000.
#' @inheritParams get_models
#' @return If \code{data} is a data frame, a \code{.csv} file or a \code{.json} file, a
#'  list with the following components:
#' \itemize{
#'   \item \code{model} A length one character vector containing the \code{model_name}
#'   \item \code{outputs} A data frame containing the prediction results for \code{data}
#' }
#'
#'  If \code{data} is a compressed file (\code{.zip}), a compressed \code{.json} file saved
#'  to \code{path} and an invisible 200 HTTP status code. If uncompressed and read into R,
#'  the file saved to \code{path} will be a list with the 2 components described above.
#'
#'  For regression models \code{outputs} will include a 1-column data frame with
#'  the predicted values.
#'
#'  For binary classification models \code{outputs} will include a 3-column
#'  data frame that includes the probability of class 0, the probability of
#'  class 1 and the classification class label result based on a 50\% threshold.
#' @seealso \code{\link{upload_model}}, \code{\link{predict_pmml}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Predict the entire iris data set
#' predict_pmml_batch(iris, "iris_model")
#'
#' # Predict the entire iris data set previously saved to a .json file
#' jsonlite::write_json(iris, "iris.json")
#' predict_pmml_batch("iris.json", "iris_model")
#'
#' # Predict the entire iris data set previously saved to a .csv file
#' write.csv(iris, "iris.csv", row.names = FALSE)
#' predict_pmml_batch("iris.csv","iris_model")
#'
#' # Predict the entire iris data set previously saved and compressed
#' predict_pmml_batch("iris.csv.zip", "iris_model", "iris_predictions.zip")
#' unzipped_predictions <- unzip("iris_predictions.zip")
#' jsonlite::fromJSON(unzipped_predictions)
#' }
predict_pmml_batch <- function(data, model_name, path = NULL, max_threads = NULL,
                               max_records_per_thread = 5000, ...) {

  if (class(data) != "data.frame" && (!is.character(data) || !file.exists(data))) {
    stop("Please either provide a data frame or a path to a file.")
  }
  if (length(model_name) != 1L || typeof(model_name) != "character") {
    stop("'model_name' must be a length-one character vector.")
  }
  if (!is.null(max_threads) && (length(max_threads) != 1 || typeof(max_threads) != "double")) {
    stop("'max_threads' must be a length-one numeric vector.")
  }
  if (length(max_records_per_thread) != 1 || typeof(max_records_per_thread) != "double") {
    stop("'max_records_per_thread' must be a length-one numeric vector.")
  }

  if (class(data) == "data.frame") {
    file <- tempfile(fileext = ".json")
    on.exit(unlink(file))
    jsonlite::write_json(data, file)
  } else {
    file <- data
  }

  query = list()
  if (!is.null(max_threads)) {
    query["maxThreads"] <- max_threads
  }
  query["maxRecordsPerThread"] <- max_records_per_thread

  url <- paste(get_zementis_base_url(), "apply" , model_name,  sep = "/") %>%
    httr::modify_url(query = query)
  my_file <- httr::upload_file(file)
  args <- list(url, httr::authenticate(get_zementis_usr(),
                                       get_zementis_pwd()),
               httr::accept_json(),
               httr::user_agent(get_useragent()),
               body = list(file = my_file),
               ...)
  is_compressed <- FALSE

  if (my_file$type == "application/zip") {
    if (is.null(path)) {
      stop("Please provide a 'path' to a file to which the predictions from Zementis Server are written to.")
    }
    is_compressed <- TRUE
    args[[length(args) + 1]] <- httr::write_disk(path, overwrite = TRUE)
  }
  response <- do.call(httr::POST, args)

  if (httr::http_error(response)) {
    error_message <- sprintf(
      "Zementis Server API request failed [%s]\n%s\n%s\n%s",
      httr::status_code(response),
      httr::http_status(response)$category,
      httr::http_status(response)$reason,
      httr::http_status(response)$message
    )
    if (httr::status_code(response) %in% c(400, 404)) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }
  if (is_compressed) {
    return(invisible(httr::status_code(response)))
  }
  httr::content(response, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
}
