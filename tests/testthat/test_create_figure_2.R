#' Unit Tests for figure 2 function
#' @description This tests checks that the functions runs with no errors and is of type ggplot.

context("Tests that figure 2 is producing the correct plot")

year_to_date = "Dec"
data <- eu_non_eu_total("2002-03-01", "2018-12-01", "quarter")

tmp_arr <-data.frame("Region" = c("EU15", "EU2", "EU8", "non_EU"),
                     'value' = c(161908, 167601, 84646, 213852))

test_that("Functions runs correctly with different year end dates", {
  expect_silent(create_figure_2(data, year_to_date, FALSE))
  expect_silent(create_figure_2(data, "Mar", FALSE))
  expect_silent(create_figure_2(data, "Jun", FALSE))
  expect_silent(create_figure_2(data, "Sep", FALSE))
})

test_that("Figure is of type ggplot and is labelled correctly", {
  p <- create_figure_2(data, year_to_date)
  expect_equal(class(p), c('gg', 'ggplot'))

  expect_error(create_figure_2(data, "Jul"))

  expect_identical(p$labels$y, "value")
  expect_identical(p$labels$x, "x")
  expect_identical(p$labels$fill, "Region")

})


test_that("Figure is of type plotly if interactive is TRUE", {
  pp <- create_figure_2(data, year_to_date, FALSE, TRUE)
  expect_equal(class(pp), c("plotly", "htmlwidget"))

})

test_that("Check convert_pie_data returns data.frame and has correct dimensions", {
  temp_data <- convert_pie_data(tmp_arr, "value", "Region")
  expect_equal(nrow(temp_data), nrow(tmp_arr))
  expect_equal(ncol(temp_data), ncol(tmp_arr)+1)
})
