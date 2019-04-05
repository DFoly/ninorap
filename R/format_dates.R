#' This function formats dates for year end filtering calculations needed to produce figure 1.
#'
#' @param data raw data obtained from the API
#' @return returns the formatted dates vector
#' @examples
#' \dontrun{format_dates (data)}


format_dates <- function(data) {

  num2month <- c("Jan","Feb","Mar",
                 "Apr","May","Jun",
                 "Jul","Aug","Sep",
                 "Oct","Nov","Dec")
  month_keys <- as.character(lubridate::month(data$dates))
  year_keys <- as.character(lubridate::year(data$dates))
  N = length(month_keys)

  year_keys <- substring(year_keys, 3, 4)

  # Convert number to month
  quarter_dates <- rep(0, N)
  row  = 1
  for (i in month_keys) {
    quarter_dates[row] <- num2month[as.numeric(i)]
    row = row + 1
  }

  formatted_dates <- paste(quarter_dates, year_keys, sep=' ')

  return (formatted_dates)
}
