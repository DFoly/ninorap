#' @title Create Figure 1
#' @description This function produces Figure 1 from the publication.
#' Plots total number of Nino Registrations from EU, outside the EU and the total.
#' Assumes data is of type list by default and data frame is the first element of the list.
#' @param data raw data from API call
#' @param year_to_date Last month in quarter: March, June, September, December
#' @param save Whether to save the plot or not. TRUE by default
#' @return year to data sums to plot figure 1
#' @examples
#' \dontrun{create_figure_1(data, year_to_date)}
#' @export

create_figure_1 <- function(data, year_to_date, save=TRUE, interactive=FALSE) {

  data <- year_to_date_sum(data, year_to_date)
  year_end_data <- dplyr::filter(data[[1]], grepl(year_to_date, data[[1]]$quarter_dates))
  N = dim(year_end_data[1])[1]

  # Scale data for plotting
  year_end_data[,3:dim(year_end_data)[2]] <- year_end_data[,3:dim(year_end_data)[2]]/1000

  # for geom_text()
  # change hardcode values to extract labels
  eu_min <- min(year_end_data$European_Union)/10
  non_eu_min <- min(year_end_data$non_eu)/10
  total_min <- min(year_end_data$Total)/10
  eu_latest <- as.data.frame(year_end_data$European_Union[N])
  non_eu_latest <- as.data.frame(year_end_data$non_eu[N])
  total_latest <- as.data.frame(year_end_data$Total[N])

  x_len = length(year_end_data$quarter_dates)

  g <- ggplot2::ggplot(year_end_data, ggplot2::aes(x = quarter_dates, y = European_Union,
                                                   text = paste('Registrations: ', round(European_Union), '\n' ,
                                                                'Date: ', quarter_dates))) +
           ggplot2::geom_line(ggplot2::aes(group = 1),color = "#2E358B", size = 1) +
           ggplot2::geom_line(ggplot2::aes(y = non_eu, group = 1), color = "#F47738", size = 1) +
           ggplot2::geom_line(ggplot2::aes(y = Total, group = 1),linetype = "dashed",color = "grey", size = 1) +
        theme_gov(base_size = 12, base_colour = "gray60") + ggplot2::labs(x = "12 months ending", y = 'Registrations in Thousands')

  # check growth rates
  eu_shift = ifelse(year_end_data$European_Union[N] - year_end_data$European_Union[N-1]>0,1,-1)
  non_eu_shift =  ifelse(year_end_data$non_eu[N] - year_end_data$non_eu[N-1]>0,1,-1)
  total_shift =  ifelse(year_end_data$Total[N] - year_end_data$Total[N-1]>0,1,-1)

  graph <- g

  # Save as png
  if (save==TRUE){
    ggplot2::ggsave("nino_registrations.png", plot = graph, width = 20, height = 10)
  }

  if (interactive==TRUE){
    # Create annotations options: may need to change yanchor depending on latest data point
    eu_annotate<- list(
      xref = 'paper',
      x = 0.95,
      y = round(eu_latest),
      xanchor = 'right',
      yanchor = 'middle',
      text = paste('Registrations from EU: ',  round(eu_latest)),
      font = list(family = 'Arial',
                  size = 10,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)

    non_eu_annotate<- list(
      xref = 'paper',
      x = 0.95,
      y = round(non_eu_latest),
      xanchor = 'right',
      yanchor = 'bottom',
      text = paste('Registrations from Non EU: ',  round(non_eu_latest)),
      font = list(family = 'Arial',
                  size = 10,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)

    total_annotate<- list(
      xref = 'paper',
      x = 0.95,
      y = round(total_latest),
      xanchor = 'right',
      yanchor = 'bottom',
      text = paste('Total Registrations: ',  round(total_latest)),
      font = list(family = 'Arial',
                  size = 10,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)



    graph <- plotly::ggplotly(p = graph, tooltip = "text") %>%
                  plotly::layout(title = "Nino Registrations",
                                     autosize = TRUE,
                                     showlegend = FALSE,
                                     annotations = eu_annotate)  %>%
                  plotly::layout(annotations = non_eu_annotate) %>%
                  plotly::layout(annotations = total_annotate)
  }
  else {
    # Adds last data point as text
    graph <- g + ggplot2::annotate("text", x=x_len, y = eu_latest[[1]]+eu_min*(eu_shift), label=paste0("EU: ",round(eu_latest)," "), color = "#2E358B") +
      ggplot2::annotate("text", x=x_len, y = non_eu_latest[[1]]+non_eu_min*(non_eu_shift), label=paste0("Non-EU: ",round(non_eu_latest)," "), color = "#F47738") +
      ggplot2::annotate("text", x=x_len, y = total_latest[[1]]+total_min*(total_shift), label=paste0("Total: ",round(total_latest)," "), color = "grey")

  }

  return(graph)

}

#data = test_data
#pp <- create_figure_1(data, "Sep", FALSE, TRUE)

