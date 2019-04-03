#' @title Create Figure 1
#' @description This function produces Figure 1 from the publication.
#' Plots total number of Nino Registrations from EU, outside the EU and the total.
#' Assumes data is of type list by default and data frame is the first element of the list.
#' @param data data to plot
#' @param year_to_date Last month in quarter: March, June, September, December
#' @return year to data sums to plot figure 1
#' @examples
#' create_figure_1(data)

create_figure_1 <- function(data, year_to_date) {

  year_end_data <- dplyr::filter(data[[1]], grepl(year_to_date, data[[1]]$quarter_dates))
  N = dim(year_end_data[1])[1]

  # for geom_text()
  eu_min <- min(year_end_data$European_Union)/10
  non_eu_min <- min(year_end_data$non_eu)/10
  total_min <- min(year_end_data$Total)/10
  eu_latest <- as.data.frame(year_end_data$European_Union[N])
  non_eu_latest <- as.data.frame(year_end_data$non_eu[N])
  total_latest <- as.data.frame(year_end_data$Total[N])

  g <- ggplot2::ggplot(year_end_data, ggplot2::aes(x = quarter_dates, y =European_Union)) +
           ggplot2::geom_line(ggplot2::aes(group = 1),color = "#2E358B", size = 1) +
           ggplot2::geom_line(ggplot2::aes(y = non_eu, group = 1), color = "#F47738", size = 1) +
           ggplot2::geom_line(ggplot2::aes(y = Total, group = 1),linetype = "dashed",color = "grey", size = 1)

  # Adds last data point as text
  graph <- g + ggplot2::annotate("text", x=17, y = eu_latest[[1]]-eu_min, label=paste0("EU: ",eu_latest," "), color = "#2E358B") +
           ggplot2::annotate("text", x=16, y = non_eu_latest[[1]]+non_eu_min, label=paste0("Non-EU: ",non_eu_latest," "), color = "#F47738") +
           ggplot2::annotate("text", x=17, y = total_latest[[1]]-total_min, label=paste0("Total: ",total_latest," "), color = "grey") +
           govstyle::theme_gov(base_size = 12, base_colour = "gray60") + ggplot2::labs(x = "12 months ending", y = 'Registrations in Thousands')

  #Fix axis labels

  # Save as png
  ggplot2::ggsave("nino_registrations.png", plot = graph, width = 20, height = 10)
  return(graph)

}


create_figure_1(data, "Dec")

