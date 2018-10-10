<!-- README.md is generated from README.Rmd. Please edit that file -->
zementisr
=========

Overview
--------

zementisr is an R client for the Zementis Server API. Zementis Server is an execution engine for PMML models which also comes with model management capabilities. Using zementisr, data scientists can deploy PMML models, predict new values by sending data to the server and manage the entire PMML model life cycle without leaving their preferred R development environment.

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("alex23lemm/zementisr")
```

Authentication
--------------

Zementis Server's REST API uses HTTP Basic Authentication. For each request the client needs to provide username and password.

The zementisr package requires that you store your secrets and the base URL of your Zementis Server as environment variables in the `.Renviron` file in your home directory.

Please, make sure to set the following environment variables accordingly in your `.Renviron` file before using functions from the zementisr package.:

``` r
ZEMENTIS_base_url = "[address]:[port]"
ZEMENTIS_usr = "[your_username]"
ZEMENTIS_pwd = "[your_password]"
```
