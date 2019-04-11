#' @title Transform data figure 3
#' @description Takes in data returned from API call and maninulates into correct form for figure 3
#' @param data raw data returned from the API call
#' @param year_to_date one of Mar, Jun, Sep, Dec
#' @param year_end_filter column to extract: e.g. quarter_dates
#' @param labels list of columns we want to extract
#' @return filtered data for plotting
#' @examples
#' \dontrun{extract_year_end_data(data, date_filter, labels)}


transform_data_figure_3 <- function(data, year_to_date, year_end_filter, labels){

  data <- year_to_date_sum(data, year_to_date)
  year_end_data <- dplyr::filter(data[[1]], grepl(year_to_date, unlist(data[[1]][2])))
  N = dim(year_end_data[1])[1]

  names.use <- names(year_end_data)[(names(year_end_data) %in% labels)]

  new_data<- year_end_data[, names.use]
  new_data <- cbind.data.frame(year_end_data[year_end_filter], new_data)

  # country name lookup
  column_names = label_lookup[names.use]

  store <- c()
  counter = 2
  store[1] <- year_end_filter
  for (i in column_names) {
    store[counter] <- i
    counter = counter + 1
  }

  colnames(new_data) <- store

  new_data <- reshape2::melt(new_data, id.vars = year_end_filter, measure.vars = c("EU15", "EU8", "EU2"))
  new_data <- new_data[order(new_data[year_end_filter]), ]


  return(new_data)
}


#dd <- transform_data_figure_3(data, "Dec", "quarter_dates", labels = c("European_Union_EU15",
#                                              "European_Union_EU2", "European_Union_EU8"))

