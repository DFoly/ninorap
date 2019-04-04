context("Tests for eu_non_eu_data function: runs without any errors?")

start_date <- "2002-03-01"
end_date <- "2016-12-01"
frequency <- "quarter"
frequency_vec <- c("quarter", "month", "annual")

test_that("Functions runs correctly", {
  expect_silent(eu_non_eu_total(start_date, end_date, frequency))
})


test_that("Dates are formated correctly and start and end dates are less then current date", {
  expect_equal(nchar(start_date), 10)
  expect_that(start_date <= Sys.Date() & end_date <= Sys.Date(), equals(TRUE))
})

test_that("Frequency is correctly formatted", {
  expect_that(frequency %in% frequency_vec, equals(TRUE))
})

test_that("Check that we return a list object", {
  expect_that(typeof(eu_non_eu_total(start_date, end_date, frequency)), equals("list"))
})

