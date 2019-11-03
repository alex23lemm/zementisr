## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
set.seed(42)
iris <- iris[sample(nrow(iris)), ]
index <- caTools::sample.split(iris$Species, 0.8)
training_set <- iris[index, ]
test_set_iris <- iris[!index, ]

iris_cart <- rpart::rpart(Species~., data = training_set)

## ------------------------------------------------------------------------
index <- caTools::sample.split(mtcars$mpg, 0.8)
training_set <- mtcars[index, ]
test_set_mtcars <- mtcars[!index, ]

mtcars_lm <- lm(mpg ~ wt + qsec + am, data = training_set)

## ----message=FALSE-------------------------------------------------------
library(pmml)
library(zementisr)

pmml(iris_cart, model_name = "iris_model") %>%
  upload_model

pmml(mtcars_lm, model_name = "mtcars_model") %>%
  upload_model

## ------------------------------------------------------------------------
get_model_metrics("iris_model")

get_model_metrics("mtcars_model")

## ------------------------------------------------------------------------
index <- sample(1:dim(test_set_iris)[1], 500, replace = TRUE)
iris_predictions <- predict_pmml_batch(test_set_iris[index, ], "iris_model")
head(iris_predictions$outputs, 4)

index <- sample(1:dim(test_set_mtcars)[1], 500, replace = TRUE)
mtcars_predictions <- predict_pmml_batch(test_set_mtcars[index, ], "mtcars_model")
head(mtcars_predictions$outputs, 4)

## ------------------------------------------------------------------------
iris_metrics <- get_model_metrics("iris_model")
iris_metrics

## ------------------------------------------------------------------------
mtcars_metrics <- get_model_metrics("mtcars_model")
mtcars_metrics

## ----fig.width=6---------------------------------------------------------
library(ggplot2)

iris_metrics[["memory_metrics"]] %>% 
  tidyr::gather("metric", "MB") %>% 
  ggplot(aes(metric, MB)) + 
    geom_col(aes(fill = metric)) + 
    coord_flip() + 
    labs(title = paste("Memory metrics for", iris_metrics[["model_name"]]))

## ----fig.width=6---------------------------------------------------------
mtcars_metrics[["memory_metrics"]] %>% 
  tidyr::gather("metric", "MB") %>% 
  ggplot(aes(metric, MB)) + 
    geom_col(aes(fill = metric)) + 
    coord_flip() + 
    labs(title = paste("Memory metrics for", mtcars_metrics[["model_name"]]))

## ----fig.width=6---------------------------------------------------------
iris_metrics[["prediction_metrics"]] %>% 
  tidyr::gather("Class", "Number of cases") %>% 
  ggplot(aes(Class, `Number of cases`)) + 
    geom_col(aes(fill = Class)) + 
    coord_flip() + 
    labs(title = paste("Model prediction metrics for", iris_metrics[["model_name"]]),
         subtitle = paste("Predictions calcuated since model deployment:",
                          rowSums(iris_metrics[["prediction_metrics"]])))

## ----fig.height=3, fig.width=6-------------------------------------------
mtcars_metrics[["prediction_metrics"]] %>%
  ggplot(aes(x = "")) +
    stat_identity(
      aes(
        lower = FirstQuartile,
        upper = ThirdQuartile,
        middle = Median,
        ymin = Minimum,
        ymax = Maximum
        ),
      geom = "boxplot") +
    coord_flip() +
    labs(
      title = paste("Model prediction metrics for", mtcars_metrics[["model_name"]])
    ) +
    xlab(NULL) +
    ylab("Miles per Gallon (MPG)")
    

## ------------------------------------------------------------------------
deactivate_model("iris_model")

## ------------------------------------------------------------------------
get_model_metrics("iris_model")

## ------------------------------------------------------------------------
activate_model("iris_model")
get_model_metrics("iris_model")

## ----results="hide"------------------------------------------------------
predict_pmml_batch(test_set_iris[1:5, ], "iris_model")

## ------------------------------------------------------------------------
get_model_metrics("iris_model")

## ----include=FALSE-------------------------------------------------------
delete_model("iris_model")
delete_model("mtcars_model")

