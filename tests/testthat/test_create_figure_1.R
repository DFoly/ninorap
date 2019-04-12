#' Unit Tests for figure 1 function
#' @description This tests checks that the functions runs with no errors and is of type ggplot.

context("Tests that figure 1 is producing the correct plot")

year_to_date = "Dec"
data = test_data


test_that("Functions runs correctly with different year end dates", {
  expect_silent(create_figure_1(data, year_to_date, FALSE))
  expect_silent(create_figure_1(data, "Mar",FALSE))
  expect_silent(create_figure_1(data, "Jun", FALSE))
  expect_silent(create_figure_1(data, "Sep", FALSE))
})


test_that("Functions should fail test", {
  expect_error(create_figure_1(data, "Jan"))
})

test_that("Figure is of type ggplot and is labelled correctly", {

  p <- create_figure_1(data, year_to_date)
  expect_equal(class(p), c('gg', 'ggplot'))

  expect_error(create_figure_1(data, "Jul"))

  expect_identical(p$labels$y, "Registrations in Thousands")
  expect_identical(p$labels$x, "12 months ending")

})




