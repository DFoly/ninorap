#' Unit Tests for extract_year_end_data
#' @description Takes in data from API call tests if function runs correctly
context("Tests that extract_year_end_data works")

year_to_date = "Dec"
year_end_filter = "quarter_dates"
labels = c("European_Union_EU15","European_Union_EU2", "European_Union_EU8")
fail_labels = c("European_Union_EU15", "Brazil", "UKJ")
data <- test_data


test_that("Functions runs correctly", {
  expect_silent(transform_data_figure_3(data, year_to_date, year_end_filter, labels))
})

test_that("Testing incorrect country labels", {
  expect_error(transform_data_figure_3(data, year_to_date, year_end_filter, fail_labels))
})
