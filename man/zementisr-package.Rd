% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zementisr.R
\docType{package}
\name{zementisr-package}
\alias{zementisr}
\alias{zementisr-package}
\title{zementisr: R client for the 'Zementis Server' API}
\description{
\if{html}{\figure{logo.png}{options: align='right' alt='logo' width='120'}}

R client for the 'Zementis Server' API <https://softwareag.com/zementis>. 
    'Zementis Server' is an execution engine for PMML models which also comes with model management
    capabilities. Using zementisr, data scientists can deploy PMML models to 'Zementis Server',
    predict new values by sending data to the server and manage the entire PMML model
    life cycle without leaving their preferred R development environment.
}
\section{Authentication}{

Zementis Server's REST API uses HTTP Basic Authentication. For each request the client needs to provide
username and password. The zementisr package requires that you store your secrets and the base URL of
your Zementis Server as environment variables in the \code{.Renviron} file in your home directory.

Please, make sure to set the environment variables below in your \code{.Renviron} file before
using functions from the zementisr package. You can easily edit \code{.Renviron} using \code{usethis::edit_r_environ()}.

\env{ZEMENTIS_base_url = "[address]:[port]"}\cr
\env{ZEMENTIS_usr = "[your_username]"}\cr
\env{ZEMENTIS_pwd = "[your_password]"}\cr
}

\seealso{
Useful links:
\itemize{
  \item \url{https://alex23lemm.github.io/zementisr}
  \item \url{https://github.com/alex23lemm/zementisr}
  \item Report bugs at \url{https://github.com/alex23lemm/zementisr/issues}
}

}
\author{
\strong{Maintainer}: Alexander Lemm \email{alex23lemm@gmail.com}

}
\keyword{internal}
