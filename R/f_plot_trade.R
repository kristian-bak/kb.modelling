#' Plotting the trades
#' @param test_data data.table with test data.
#' @param dt_transactions data.table with transactions. This should be outcome from f_trade.
#' @export

f_plot_trade <- function(test_data, dt_transactions) {

  my_return <- dt_transactions$stats$total_yield
  hodl_buy <- dt_transactions$transactions$buy_value[1]
  hodl_sell <- test_data[.N, Close]
  hodl_return <- round(100 * (hodl_sell - hodl_buy) / hodl_buy, 3)
  str_text1 <- paste0("My return: ", my_return, " %")
  str_text2 <- paste0("Hodl return: ", hodl_return, " %")
  str_text <- paste0(c(str_text1, "<br>", str_text2), collapse = " ")

  buy_day <- dt_transactions$transactions$buy_date[1]
  start_day <- as.Date(test_data[1, Date])
  end_day <- as.Date(test_data[.N, Date])
  text_day <- start_day + floor((as.numeric(end_day - start_day)) / 2)
  y_val <- max(c(test_data$Open, test_data$High, test_data$Low, test_data$Close), na.rm = TRUE)
  a <- list(x = text_day,
            y = y_val,
            text = str_text,
            showarrow = FALSE)

  test_data[, Time := as.Date(Date)]
  plotly::plot_ly(data = test_data, x = ~Time, type = "candlestick",
          open = ~Open, close = ~Close, low = ~Low, high = ~High) %>%
    plotly::add_annotations(x = dt_transactions$transactions$buy_date,
                    y = dt_transactions$transactions$buy_value,
                    arrowcolor = 'black',
                    xref = "x",
                    yref = "y",
                    axref = "x",
                    ayref = "y",
                    text = "Buy",
                    showarrow = TRUE,
                    ax = dt_transactions$transactions$buy_date,
                    ay = dt_transactions$transactions$buy_value * 0.9) %>%
    plotly::add_annotations(x = dt_transactions$transactions$sell_date,
                    y = dt_transactions$transactions$sell_value,
                    arrowcolor = 'black',
                    xref = "x",
                    yref = "y",
                    axref = "x",
                    ayref = "y",
                    text = "Sell",
                    showarrow = TRUE,
                    ax = dt_transactions$transactions$sell_date,
                    ay = dt_transactions$transactions$sell_value * 0.9) %>%
    plotly::layout(title = test_data[1, Ticker],
           xaxis = list(rangeslider = list(visible = FALSE)),
           annotations = a)

}
