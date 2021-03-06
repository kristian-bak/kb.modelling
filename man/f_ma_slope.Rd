% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_indicator.R
\name{f_ma_slope}
\alias{f_ma_slope}
\title{This function calculates the slope of a moving average based on n data points}
\usage{
f_ma_slope(data, n = 10)
}
\arguments{
\item{data}{data.table. data should be a obj an outcome from f_indicators to ensure MA is present in data.}

\item{n}{number of days used to calculate the slope}
}
\value{
data.table object with moving average slope for each day
}
\description{
This function calculates the slope of a moving average based on n data points
}
