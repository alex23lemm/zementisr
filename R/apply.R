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
#' @seealso [upload_model()]
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  server_prediciton <- apply_model("iris_model", iris[1, ])
#' }

apply_model <- function(model_name, x) {

  url <- paste(get_zementis_base_url(),
               "apply",
               gsub(" ", "%20", model_name),
               sep = "/") %>%
    httr::modify_url(query = list(record = toJSON(x)))

  response <- httr::GET(url, httr::authenticate(get_zementis_usr(),
                                                get_zementis_pwd()),
                        content_type_json(),
                        httr::user_agent(get_useragent()))

  parsed <- content(response, as = "text") %>% jsonlite::fromJSON()

}
