#' Unit Tests for year to date sum.
#' @description This tests checks for correctly formatted paramters.

context("Year to date sum works correctly")

year_to_date <- "Mar"
quarters <- c("Mar", "Jun", "Sep", "Dec")


test_that("Year to data parameter is correct", {
  expect_that(year_to_date %in% quarters, equals(TRUE))
})

test_that("Year to data parameter is incorrect", {
  year_to_date = "Feb"
  expect_that(year_to_date %in% quarters, equals(FALSE))
})

test_that("Functions runs correctly", {
  expect_silent(year_to_date_sum(ninorap::test_data, "Mar"))
})

test_that("Functions runs incorrectly", {
  expect_error(year_to_date_sum(ninorap::test_data, "Feb"))
})

test_that("Year to data parameter is correct", {
  data <- year_to_date_sum(ninorap::test_data, "Dec")
  expect_that(year_to_date %in% quarters, equals(TRUE))
})


test_that("Test sum is correct December", {
  temp_data <- ninorap::test_data
  data <- year_to_date_sum(ninorap::test_data, "Dec")
  # since Dec first 4 entries should be the same as total
  sum_cal = sum(temp_data[1][[1]][1][1:4,])
  sum_data = data[1][[1]]$European_Union[4]
  expect_equal(sum_cal, sum_data)


})

test_that("Test sum is correct March", {
  temp_data <- ninorap::test_data
  data <- year_to_date_sum(ninorap::test_data, "Mar")
  # since Dec first 4 entries should be the same as total
  sum_cal = sum(temp_data[1][[1]][2][1,])
  sum_data = data[1][[1]]$European_Union_EU15[1]
  expect_equal(sum_cal, sum_data)

})

test_that("Test sum is correct June", {
  temp_data <- ninorap::test_data
  data <- year_to_date_sum(ninorap::test_data, "Jun")
  # since Dec first 4 entries should be the same as total
  sum_cal = sum(temp_data[1][[1]][2][1:2,])
  sum_data = data[1][[1]]$European_Union_EU15[2]
  expect_equal(sum_cal, sum_data)

})

