
context("Tests that table 5 returns correct data")

year_to_date = "Dec"
year_to_date2 = "Mar"
year_to_date3  = "Nov"



test_that("Functions runs correcty and returns 5 results", {
  df  = top_5_table("2018-12-01",year_to_date)
  expect_equal(dim(df)[1], 5)
  df1 = top_5_table("2018-12-01", year_to_date2)
  expect_equal(dim(df1)[1], 5)
  df2 = top_5_table("2018-12-01", year_to_date3)
  expect_error(dim(df2)[1] == 5)

})

