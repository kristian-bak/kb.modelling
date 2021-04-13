
## Document
devtools::document()

## Load function:
devtools::load_all()

## Install
devtools::install(upgrade = FALSE)

## Build

devtools::build()

## Test all functions:
devtools::test()

## Test one function:
devtools::test(filter = "f_load_one")

## Create test
usethis::use_test(name = "f_load_one")

## Check everything:
devtools::check()

test_check(package = "kb.modelling")

## df_symbols skal fikses

devtools::install("C:/Users/Kristian/Git/kb.modelling", upgrade = FALSE)

usethis::use_mit_license()

## Update dependencies
usethis::use_package("xgboost")
usethis::use_package("data.table")
usethis::use_package("quantmod")
usethis::use_package("magrittr")
usethis::use_package("TTR")
usethis::use_package("rvest")
usethis::use_package("xml2")
usethis::use_package("Ckmeans.1d.dp")

## Bump version number
usethis::use_version()

