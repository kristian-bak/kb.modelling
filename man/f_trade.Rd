% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_trade.R
\name{f_trade}
\alias{f_trade}
\title{Stock trading based on xgboost predictions}
\usage{
f_trade(test_data, buy_cut = 0.6, sell_cut = 0.4)
}
\arguments{
\item{test_data}{data. Should be a data.table object with predictions from f_predict}

\item{buy_cut}{cut off determining when to buy. Should be a number between 0 and 1}

\item{sell_cut}{cut off determining when to sell. Should be a number between 0 and 1}
}
\value{
list with trades made and earnings
}
\description{
Stock trading based on xgboost predictions
}
