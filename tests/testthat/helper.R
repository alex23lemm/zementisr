# Create new PMML models which will be deployed to Zementis Server
data(kyphosis, package = "rpart")
kyphosis_fit <- rpart::rpart(Kyphosis ~ Age + Number + Start, data = rpart::kyphosis)
kyphosis_pmml <- pmml::pmml(kyphosis_fit, model_name = "kyphosis_model")
XML::saveXML(kyphosis_pmml, "kyphosis_pmml.xml")

iris_fit <- lm(Sepal.Length ~ ., data=iris)
iris_pmml <- pmml::pmml(iris_fit, model_name = "iris_model")
XML::saveXML(iris_pmml, "iris_pmml.xml")

err_not_known <- paste("Zementis Server API request failed [404]",
                       "Client error",
                       "Not Found",
                       "Client error: (404) Not Found",
                       "Model 'unknown_model' not found.", sep = "\n")
