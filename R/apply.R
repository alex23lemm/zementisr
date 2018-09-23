#' Apply PMML model to data
#'
#' Apply model to single input record
#'
#' @param model_name The name of the deployed PMML model which will predict
#'  the \code{record}.
#' @param x A one row data frame containg the data set which will be send to
#'  Zementis Server for prediction. The data frame column names must match
#'  the PMML model argument names.
#' @return A list with the following components:
#' \itemize{
#'   \item \code{model} A vector containg the \code{mode_name}
#'   \item \code{outputs} A data frame containing the prediction results
#' }
#'
#'  For regression models \code{outputs} will include a 1-column data frame with
#'  the predicted value.
#'
#'  For binary classification models \code{outputs} will include a 3-column
#'  data frame that includes the probability of class A, the probability of
#'  class B and the classification label based on a 50\% threshold.
#' @seealso [upload_model]
#' @export
#'
#' @examples
#' \dontrun{
#' apply_model("iris_model", iris[1, ])
#' }

apply_model <- function(model_name, x) {

  url <- paste(get_zementis_base_url(),
               "apply",
               gsub(" ", "%20", model_name),
               sep = "/") %>%
    httr::modify_url(query = list(record = jsonlite::toJSON(x)))

  response <- httr::GET(url, httr::authenticate(get_zementis_usr(),
                                                get_zementis_pwd()),
                        #httr::content_type_json(),
                        httr::user_agent(get_useragent()))

  parsed <- httr::content(response, as = "text") %>%
    jsonlite::fromJSON()

  parsed

}
