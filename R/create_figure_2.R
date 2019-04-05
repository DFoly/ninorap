#' @title create_figure_2
#' @description This function produces Figure 2 (pie chart) in the Nino Statistical Publication
#' @param data data to plot
#' @param year_to_date Last month in quarter: one of Mar, Jun, Sep, Dec
#' @return pie chart showing number of registrations from EU vs Non-EU
#' @examples
#' \dontrun{create_figure_2(data, year_to_date)}
#' @export

create_figure_2 <- function(data, year_to_date) {

  year_end_data <- dplyr::filter(data[[1]], grepl(year_to_date, data[[1]]$quarter_dates))
  N = dim(year_end_data[1])[1]


  # Create matrix for data to plot
  tmp <- c("European_Union_EU15", "European_Union_EU2", "European_Union_EU8", "non_eu")
  tmp_arr = matrix(0, nrow = length(tmp), ncol = 2)
  counter = 1
  for (i in tmp) {

    tmp_arr[counter,] <- c(tmp[counter], year_end_data[[i]][N])
    counter = counter + 1
  }
  tmp_arr <- as.data.frame(tmp_arr)
  colnames(tmp_arr) <- c("Region", "value")

  # sorting by name ensures categories in same position on pie chart
  # important for labelling
  tmp_arr <- tmp_arr %>% dplyr::arrange(Region)

  # Create a basic bar
  pie = ggplot2::ggplot(tmp_arr, ggplot2::aes(x="", y=value, fill=Region)) + ggplot2::geom_bar(stat="identity", width=1, colour = "white")

  # Convert to pie (polar coordinates) and add labels
  pie = pie + ggplot2::coord_polar("y", start=0) +

              ggplot2::geom_text(ggplot2::aes(label = c("EU15", "EU2","EU8", "Non EU")), position = ggplot2::position_stack(vjust = 0.5),
                                                                      fontface = "bold", colour = "white") +
                                                                      govstyle::theme_gov(base_size = 12, base_colour = "gray60") +
              ggplot2::scale_fill_manual(values=c("non_eu" = "#F47738", "European_Union_EU15" = "#005EA5","European_Union_EU8" = "#005EA5","European_Union_EU2"="#005EA5"))+

              # get rid of axis and value labels
              ggplot2::theme(axis.line=ggplot2::element_blank(),
                    axis.text.x=ggplot2::element_blank(),
                    axis.text.y=ggplot2::element_blank(),
                    axis.ticks=ggplot2::element_blank(),
                    axis.title.x=ggplot2::element_blank(),
                    axis.title.y=ggplot2::element_blank(),
                    legend.position="none",
                    panel.background=ggplot2::element_blank(),
                    panel.border=ggplot2::element_blank(),
                    panel.grid.major=ggplot2::element_blank(),
                    panel.grid.minor=ggplot2::element_blank(),
                    plot.background=ggplot2::element_blank())

  ggplot2::ggsave("eu_non_eu_pie.png", plot = pie, width = 20, height = 10)
  return(pie)

}


#pie <- create_figure_2(data, year_to_date)
