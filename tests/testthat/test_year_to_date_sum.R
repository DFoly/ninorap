#' Unit Tests for year to date sum.
#' @description This tests checks for correctly formatted paramters.

context("Year to date sum works correctly")

year_to_date <- "Mar"
quarters <- c("Mar", "Jun", "Sep", "Dec")


test_that("Year to data parameter is correct", {
  expect_that(year_to_date %in% quarters, equals(TRUE))
})


test_that("Functions runs correctly", {
  expect_silent(year_to_date_sum(ninorap::test_data, "Mar"))
})

test_that("Functions runs correctly", {
  expect_error(year_to_date_sum(ninorap::test_data, "Feb"))
})

