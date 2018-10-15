## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  ZEMENTIS_base_url = "http://localhost:9083"
#  ZEMENTIS_usr = "guybrush.threepwood"
#  ZEMENTIS_pwd = "bigwhoop"
#  

## ----message=FALSE-------------------------------------------------------
library(rpart)
library(pmml)

iris_lm <- lm(Sepal.Length ~ ., data=iris)
iris_pmml <- pmml(iris_lm, model.name = "iris_model")
saveXML(iris_pmml, "iris_pmml.xml")

kyphosis_fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
kyphosis_pmml <- pmml(kyphosis_fit, model.name = "kyphosis_model")

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
apply_model(iris[42, ], "iris_model")
apply_model(kyphosis[23, ], "kyphosis_model")

## ------------------------------------------------------------------------
apply_model_batch(iris[23:25, ], "iris_model")
jsonlite::write_json(iris[23:25, ], "iris.json")
apply_model_batch("iris.json", "iris_model")
write.csv(iris[23:25, ], "iris.csv", row.names = FALSE)
apply_model_batch("iris.csv","iris_model")

## ------------------------------------------------------------------------
delete_model("iris_model")
delete_model("kyphosis_model")

