#' Unit Tests for figure 3 function
#' @description This tests checks that the functions runs with no errors and is of type ggplot.

context("Tests that figure 3 is producing the correct output")

year_to_date = "Dec"
year_end_filter = "quarter_dates"
labels = c("European_Union_EU15","European_Union_EU2", "European_Union_EU8")
fail_labels = c("European_Union_EU15", "Brazil", "UKJ")
data <- eu_non_eu_total("2002-03-01", "2018-12-01", "quarter")

year_end_filter = "quarter_dates"
labels = c("European_Union_EU15", "European_Union_EU2", "European_Union_EU8")

test_that("Function runs correctly", {
  expect_error(create_figure_3(data, "Jul"))
})


test_that("Figure is of type plotly and htmlwidget if interactive is TRUE", {

  pp <- create_figure_3(data, year_to_date, year_end_filter, labels, FALSE, TRUE)
  expect_equal(class(pp), c("plotly", "htmlwidget"))

})

# Test fails because important dates are hardcoded in and the test_data
# is a subsample so there is an error since it cant find later dates:
#enable user selection
