test_that("f_load_one works in offline mode", {

  data <- f_load_one(ticker = "MATAS.CO", offline = TRUE)

  ## Expecting data.table
  expect_true(is.data.table(data))

  ## Expecting rows
  expect_gt(nrow(data), 1)

  ## Expecting columns
  expect_gt(ncol(data), 10)

  ## Expecting Close and RSI variables
  expect_true(all(c("Close", "RSI") %in% names(data)))

})
