# zementisr 0.5.0 

* Update hex logo format to .png and add favicons 

* Add unit test for spell checking

# zementisr 0.4.2

* Modify README, vignettes and tests to align with R pmml package release 2.0.0 and Zementis Server release 10.5

# zementisr 0.4.1

* Internal function `get_zementis_base_url()` stops adding "adapars" to base URL fetched from `.Renviron`

# zementisr 0.4.0

## Breaking changes

* `apply_model()` renamed to `predict_pmml()` which is a more intuitive function name for the common R user

* `apply_model_batch()` renamed to `predict_pmml_batch()`

## Minor improvements

* `predict_pmml_batch()` gets two additional arguments `max_threads` and `max_records_per_thread`

# zementisr 0.3.0

* `get_model_metrics()` gets PMML model memory and model prediction metrics from Zementis Server

* The model metrics vignette (see `vignette("model-metrics")`) shows how to download and visualize metrics for different kind of predictive models from Zementis Server

# zementisr 0.2.0

* `download_model()` downloads the original PMML model source from Zementis Server

# zementisr 0.1.0

* `get_models()` lists all models deployed to Zementis Server

* `upload_model()` uploads PMML model (residing in memory or saved to a file) to Zemenits Server

* `get_model_properties()` gets PMML model name, description, input and output field properties

* `activate_model()` activates PMML model on Zementis Server

* `deactivate_model()` deactivates PMML model on Zementis Server

* `delete_model()` deletes PMML model from Zementis Server

* `apply_model()` applies PMML model on single data record that is sent to Zementis Server

* `apply_model_batch()` Applies PMML model to multiple data records (a data frame in memory or a file saved to disk) that are sent to Zementis Server

*  Read the Quickstart Guide vignette (see `vignette("zementisr")`) for an in-depth introduction to the zementisr package

