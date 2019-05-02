#' Unit Tests for figure 4 function
#' @description This tests checks that the functions runs with no errors and is of type ggplot.

context("Tests that figure 4 is producing the correct output")

year_to_date = "Dec"
year_end_filter = "quarter_dates"
labels = c("European_Union_EU15","European_Union_EU2", "European_Union_EU8", "Oceania",
           "Central_and_South_America", "North_America", "North_Africa", "Sub_Saharan_Africa",
           "South_East_Asia", "South_Asia", "East_Asia", "Middle_East_and_Central_Asia")
fail_labels = c("European_Union_EU15", "Brazil", "UKJ")
data <- eu_non_eu_total("2002-03-01", "2018-12-01", "quarter")


test_that("Function runs correctly", {
  expect_error(create_figure_4(data, "Jul"))
})

test_that("Figure is of type plotly and htmlwidget if interactive is TRUE", {

  pp <- create_figure_4(data, year_to_date, labels, FALSE, TRUE)
  expect_equal(class(pp), c("plotly", "htmlwidget"))

})
