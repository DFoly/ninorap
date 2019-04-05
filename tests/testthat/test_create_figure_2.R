#' Unit Tests for figure 2 function
#' @description This tests checks that the functions runs with no errors and is of type ggplot.


context("Tests that figure 2 is producing the correct plot")

year_to_date = "Dec"

test_that("Functions runs correctly", {
  data <- year_to_date_sum(ninorap::test_data, year_to_date)
  expect_silent(create_figure_2(data, year_to_date))
})

test_that("Figure is of type ggplot and is labelled correctly", {

  data_test <- year_to_date_sum(ninorap::test_data, year_to_date)
  p <- create_figure_2(data_test, year_to_date)
  expect_equal(class(p), c('gg', 'ggplot'))

  expect_error(create_figure_2(data, "Jul"))

  expect_identical(p$labels$y, "value")
  expect_identical(p$labels$x, "x")
  expect_identical(p$labels$fill, "Region")

})
