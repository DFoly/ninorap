#' Unit Tests for extract_year_end_data
#' @description

context("Tests that extract_year_end_date works")

year_to_date = "Dec"
year_end_filter = "quarter_dates"
labels <- c("European_Union_EU15","European_Union_EU2", "European_Union_EU8")
data <- year_to_date_sum(test_data, year_to_date)

test_that("Functions runs correctly", {
  expect_silent(extract_year_end_date(data, year_to_date, year_end_filter, labels))
})


test_that("Size of dataframe is correct", {
  df <- extract_year_end_date(data, year_to_date, year_end_filter, labels)
  expect_equal(ncol(df), 3)
  expect_equal(nrow(df), 27)
})
