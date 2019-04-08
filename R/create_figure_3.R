#' @title Create Figure 3
#' @description This function produces Figure 3 from the publication.
#' Plots total number of Nino Registrations from EU, outside the EU and the total.
#' Assumes data is of type list by default and data frame is the first element of the list.
#' @param data data to plot
#' @param year_to_date Last month in quarter: March, June, September, December
#' @return year to data sums to plot figure 1
#' @examples
#' \dontrun{create_figure_1(data, year_to_date)}
#' @export

create_figure_3 <- function(data, year_to_date){



  year_end_data <- extract_year_end_date(data, year_to_date, year_end_filter, labels)



  return ()

}
