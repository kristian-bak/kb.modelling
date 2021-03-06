% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_sfa.R
\name{f_sfa}
\alias{f_sfa}
\title{Single feature analysis}
\usage{
f_sfa(
  ticker,
  from_date = "2016-01-01",
  offline = FALSE,
  target_name,
  var_names = NULL,
  b
)
}
\arguments{
\item{ticker}{ticker name passed as string}

\item{from_date}{load data since from_date. Default is 2016-01-01.}

\item{offline}{logical. If TRUE, data will be loaded from data folder.}

\item{target_name}{target_name}

\item{var_names}{variables to loop through xgboost model. Default is NULL which means all variables from f_indicators will be used.}

\item{b}{number of bootstrap iterations}
}
\description{
Single feature analysis
}
