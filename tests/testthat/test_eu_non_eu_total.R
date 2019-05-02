#' Unit tests for calling Stat-Xplore API
#' @description This tests for correctly formatted parameters and checks that
#' the functions runs with no errors.

context("Tests for eu_non_eu_data function: runs without any errors?")

start_date <- "2014-03-01"
end_date <- "2018-12-01"
frequency <- "quarter"
frequency_vec <- c("quarter", "month", "annual")


test_that("Dates are formated correctly and start and end dates are less then current date", {
  expect_equal(nchar(start_date), 10)
  expect_that(start_date <= Sys.Date() & end_date <= Sys.Date(), equals(TRUE))
})


test_that("Correct data was retrieved: European",{
  data <- eu_non_eu_total(start_date, end_date, frequency, FALSE)
  expect_equal(length(data$colnames), 19)
})

test_that("Frequency is correctly formatted", {
  expect_that(frequency %in% frequency_vec, equals(TRUE))
})


 test_that("Correct data was retrieved: All countries", {
  data <- eu_non_eu_total(start_date, end_date, frequency, TRUE)
  expect_equal(length(data$colnames), 236)
  expect_error(eu_non_eu_total(start_date, end_date, frequency, TRUE, FALSE, 'ksjdhfksgdf'))
})

