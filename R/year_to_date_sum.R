#' This function calculates the Year to date sum for Figure 1 in the report
#' @param data raw data returned from the API call
#' @param year_to_date Last month in quarter: March, June, September, December
#' @return list containing data frame, dates and length of time series
#' @examples
#' \dontrun{year_to_date_sum (data, "Mar")}


year_to_date_sum <- function(data, year_to_date) {
  # get final date and this will define year to date
  # calculation, if June then only sum two observations
  # for each year.

  month2num <- list("Mar"= 1, "Jun" = 2, "Sep" = 3 , "Dec" = 4)
  roll_apply_param <- month2num[[year_to_date]]
  N <- dim(data$df)[1]

  df = zoo::rollapply(data$df, roll_apply_param, sum, fill = NA, by.column=TRUE, align="right")

  quarter_dates <- format_dates(data)

  df_new <- cbind.data.frame(data$dates, quarter_dates, df)

  return(list(df_new,N))
}

# Testing function
#temp <- year_to_date_sum(df_struct, "Dec")
#plot_data <- as.data.frame(temp[[1]])
#dates_temp <- temp[[2]]

# Now should be able to filter by year ending quarter
#dplyr::filter(temp[[1]], grepl('Mar', temp[[1]]$quarter_dates))

#data <-  year_to_date_sum(test_data, "Dec")
