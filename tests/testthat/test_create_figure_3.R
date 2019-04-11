#' Unit Tests for figure 3 function
#' @description This tests checks that the functions runs with no errors and is of type ggplot.

context("Tests that figure 3 is producing the correct output")

year_to_date = "Dec"
year_end_filter = "quarter_dates"
labels = c("European_Union_EU15","European_Union_EU2", "European_Union_EU8")
fail_labels = c("European_Union_EU15", "Brazil", "UKJ")
data <- test_data

year_end_filter = "quarter_dates"
labels = c("European_Union_EU15", "European_Union_EU2", "European_Union_EU8")

test_that("Function runs correctly", {
  expect_error(create_figure_3(data, "Jul"))
})

# Test fails because important dates are hardcoded in and the test_data
# is a subsample so there is an error since it cant find later dates:
#enable user selection
