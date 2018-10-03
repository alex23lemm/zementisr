#' Apply PMML model to single input record
#'
#' \code{apply_model()} returns the prediction for a single input record that is sent
#'   Zementis Server. The value returned depends on the type of prediction model
#'   being executed on the server.
#'
#' @param model_name The name of the deployed PMML model which will predict
#'  the record \code{x}.
#' @param x A one row data frame containg the data set which will be send to
#'  Zementis Server for prediction. The data frame column names must match
#'  the PMML model argument names.
#' @inheritParams get_models
#' @return A list with the following components:
#' \itemize{
#'   \item \code{model} A vector containg the \code{model_name}
#'   \item \code{outputs} A data frame containing the prediction results
#' }
#'
#'  For regression models \code{outputs} will include a 1-column data frame with
#'  the predicted value.
#'
#'  For binary classification models \code{outputs} will include a 3-column
#'  data frame that includes the probability of class 0, the probability of
#'  class 1 and the classification class label result based on a 50\% threshold.
#' @seealso \code{\link{upload_model}}, \code{\link{apply_model}}
#' @export
#'
#' @examples
#'
#' \dontrun{
#' apply_model("iris_model", iris[42, ])
#' }
apply_model <- function(x, model_name, ...) {

  if(dim(x)[1] > 1) {
    stop("Please provide a data frame with a single record.")
  }

  url <- paste(get_zementis_base_url(),
               "apply",
               gsub(" ", "%20", model_name),
               sep = "/") %>%
    httr::modify_url(query = list(record = jsonlite::toJSON(x)))

  response <- httr::GET(url, httr::authenticate(get_zementis_usr(),
                                                get_zementis_pwd()),
                        httr::content_type_json(),
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
    if(httr::status_code(response) %in% c(400, 404)) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }
  httr::content(response, as = "text") %>%
   jsonlite::fromJSON()
}

#' Apply PMML model to multiple input records
#'
#' @param model_name The name of the deployed PMML model which will predict
#'  the records contained  in \code{df} or in \code{file}
#' @param df A data frame containg multiple data records which will be sent to
#'  Zementis Server for prediction. The data frame column names must match
#'  the PMML model argument names.
#' @param file A path to a file which contains multiple data records which will be
#'   sent to Zementis Server for prediction.
#' @inheritParams get_models
#' @seealso \code{\link{upload_model}}, \code{link{apply_model}}
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Predict the entire iris data set
#' apply_model_batch("iris_model", iris)
#'
#' # Predict the entire iris data set previously saved to a .json file
#' jsonlite::write_json(iris, "iris.json")
#' apply_model_batch("iris_model", file = "iris.json")
#'
#' # Predict the entire iris data set previously saved to a .csv file
#' write.csv(iris, "iris.csv", row.names = FALSE)
#' apply_model_batch("iris_model", file = "iris.csv")
#' }
apply_model_batch <- function(model_name, df, file) {

  if(missing(df) & missing(file)) {
    stop("Please either provide a data frame or a path to a file.")
  }

  if(!missing(df)) {
    file <- tempfile(fileext = ".json")
    on.exit(unlink(file))
    jsonlite::write_json(df, file)
  }

  url <- paste(get_zementis_base_url(), "apply" , model_name,  sep = "/")

  my_file <- httr::upload_file(file)
  response <- httr::POST(url, httr::authenticate(get_zementis_usr(),
                                                 get_zementis_pwd()),
                         httr::accept_json(),
                         httr::user_agent(get_useragent()),
                         body = list(file = my_file))
  httr::content(response, as = "text") %>%
    jsonlite::fromJSON()
}
