## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  ZEMENTIS_base_url = "https://localhost:9083/adapars"
#  ZEMENTIS_usr = "guybrush.threepwood"
#  ZEMENTIS_pwd = "bigwhoop"
#  

## ----message=FALSE-------------------------------------------------------
library(rpart)
library(pmml)

iris_lm <- lm(Sepal.Length ~ ., data=iris)
iris_pmml <- pmml(iris_lm, model_name = "iris_model")
saveXML(iris_pmml, "iris_pmml.xml")

kyphosis_fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
kyphosis_pmml <- pmml(kyphosis_fit, model_name = "kyphosis_model")

## ------------------------------------------------------------------------
library(zementisr)

upload_model("iris_pmml.xml")
upload_model(kyphosis_pmml)

## ------------------------------------------------------------------------
get_models()

## ------------------------------------------------------------------------
get_model_properties("kyphosis_model")

## ------------------------------------------------------------------------
deactivate_model("iris_model")
deactivate_model("kyphosis_model")

## ------------------------------------------------------------------------
get_models() %>% purrr::map_df(activate_model)

## ------------------------------------------------------------------------
predict_pmml(iris[42, ], "iris_model")
predict_pmml(kyphosis[23, ], "kyphosis_model")

## ------------------------------------------------------------------------
predict_pmml_batch(iris[23:25, ], "iris_model")
jsonlite::write_json(iris[23:25, ], "iris.json")
predict_pmml_batch("iris.json", "iris_model")
write.csv(iris[23:25, ], "iris.csv", row.names = FALSE)
predict_pmml_batch("iris.csv","iris_model")

## ----eval=FALSE----------------------------------------------------------
#  iris_download <- download_model("iris_model")
#  XML::saveXML(iris_download[["model_source"]], file = iris_download[["model_name"]])
#  

## ------------------------------------------------------------------------

downloads <- get_models() %>% purrr::map(download_model)

tibble::tibble(
  model_name = purrr::map_chr(downloads, "model_name"), 
  source = purrr::map(downloads, "model_source"))

## ------------------------------------------------------------------------
purrr::walk2(purrr::map(downloads, "model_source"),
             purrr::map_chr(downloads, "model_name"),
             XML::saveXML)

## ------------------------------------------------------------------------
delete_model("iris_model")
delete_model("kyphosis_model")

