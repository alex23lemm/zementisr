#' Get prediction for single input record from PMML model
#'
#' \code{predict_pmml()} returns the prediction for a single input record that is sent
#' to Zementis Server. The value returned depends on the type of prediction model
#' being executed on the server.
#'
#' @param x A one row data frame containing the data record which is sent to
#'  Zementis Server for prediction. The data frame column names must match
#'  the PMML model argument names.
#' @param model_name The name of the deployed PMML model that gets the prediction
#'  on the new data record \code{x}.
#' @inheritParams get_models
#' @return A list with the following components:
#' \itemize{
#'   \item \code{model} A length one character vector containing the \code{model_name} that was executed on the server
#'   \item \code{outputs} A data frame containing the prediction results for \code{x}
#' }
#'
#'  For regression models \code{outputs} will include a 1-column data frame with
#'  the predicted value.
#'
#'  For binary classification models \code{outputs} will include a 3-column
#'  data frame that includes the probability of class 0, the probability of
#'  class 1 and the classification class label result based on a 50\% threshold.
#' @seealso \code{\link{upload_model}}, \code{\link{predict_pmml_batch}}
#' @export
#'
#' @examples
#' \dontrun{
#' predict_pmml(iris[42, ], "iris_model")
#' }
predict_pmml <- function(x, model_name, ...) {

  if (!is.data.frame(x)) {
    stop("'x' must be a data frame with a single record.")
  }
  if (is.data.frame(x) && dim(x)[1] > 1) {
    stop("'x' must be a data frame with with a single record.")
  }
  if (length(model_name) != 1L || typeof(model_name) != "character") {
    stop("'model_name' must be a length-one character vector")
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
    if (httr::status_code(response) %in% c(400, 404)) {
      error_message <- paste(error_message,
                             httr::content(response)$errors[[1]],
                             sep = "\n")
    }
    stop(error_message, call. = FALSE)
  }
  httr::content(response, as = "text", encoding = "UTF-8") %>%
   jsonlite::fromJSON()
}
