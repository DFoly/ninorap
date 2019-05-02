#' @title Create Figure 4
#' @description This function produces Figure 4 from the publication. Registration
#' by world area current year vs the previous year.
#' @param data raw data from API call
#' @param year_to_date Last month in quarter, one of: Mar, Jun, Sep, Dec
#' @param labels labels from API call must be in colnames of data
#' @param save whether or not to save figure as png Default is TRUE
#' use data$colnames to see available choices
#' @return plot of class ggplot
#' @examples
#' \dontrun{create_figure_3(data, year_to_date)}
#' @export

create_figure_4 <- function(data, year_to_date, labels, save=TRUE, interactive=FALSE) {

  # year to date and filter by world area
  data <- year_to_date_sum(data, year_to_date)
  year_end_data <- dplyr::filter(data[[1]], grepl(year_to_date, data[[1]]$quarter_dates))
  N = dim(year_end_data[1])[1]


  # Scale data for plotting
  year_end_data[,3:dim(year_end_data)[2]] <- year_end_data[,3:dim(year_end_data)[2]]/1000

  # Only want last two entries corresponding to last two years
  ncols <- dim(year_end_data)[2]
  year_end_data <- year_end_data[N:(N-1),]
  year_end_data <- cbind.data.frame(year_end_data$quarter_dates,year_end_data[,labels])
  colnames(year_end_data)[1] <- "quarter_dates"

  #reshape data
  reshaped_data <- reshape2::melt(year_end_data, id = 'quarter_dates')


  plot <- ggplot2::ggplot(data=reshaped_data, ggplot2::aes(x=variable, y=value, fill = quarter_dates)) +
    ggplot2::geom_bar(stat="identity", position = "dodge", show.legend=TRUE) + ggplot2::coord_flip() +
    ggplot2::scale_fill_manual("legend", values = c("Dec 17" = "grey", "Dec 18" = "#005EA5")) +
    theme_gov() + ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())


  if (save == TRUE) {
    ggplot2::ggsave("world_registrations.png", plot = plot, width = 10, height = 5)
  }

  if (interactive == TRUE) {
    plot <- plotly::ggplotly(plot)
  }


  return (plot)

}

#create_figure_4(data, year_to_date, labels, FALSE, TRUE)
