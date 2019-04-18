#' @title create_figure_2
#' @description This function produces Figure 2 (pie chart) in the Nino Statistical Publication
#' @param data raw data from API
#' @param year_to_date Last month in quarter: one of Mar, Jun, Sep, Dec
#' @param save Whether to save the plot or not. TRUE by default
#' @return pie chart showing number of registrations from EU vs Non-EU
#' @examples
#' \dontrun{create_figure_2(data, year_to_date)}
#' @export

create_figure_2 <- function(data, year_to_date, save=TRUE, interactive=FALSE) {

  scale = 1000

  data <- year_to_date_sum(data, year_to_date)
  year_end_data <- dplyr::filter(data[[1]], grepl(year_to_date, data[[1]]$quarter_dates))
  N = dim(year_end_data[1])[1]

  # Create matrix for data to plot
  tmp <- c("European_Union_EU15", "European_Union_EU2", "European_Union_EU8", "non_eu")
  tmp_arr = matrix(0, nrow = length(tmp), ncol = 2)
  counter = 1
  for (i in tmp) {

    tmp_arr[counter,] <- c(tmp[counter], year_end_data[[i]][N]/scale)
    counter = counter + 1
  }
  tmp_arr <- as.data.frame(tmp_arr)
  colnames(tmp_arr) <- c("Region", "value")

  # sorting by name ensures categories in same position on pie chart
  # important for labelling
  tmp_arr <- tmp_arr %>% dplyr::arrange(Region)


  # Generate interactive plotly pie chart
  if (interactive == TRUE){
        pie_data <- convert_pie_data(tmp_arr, colnames(tmp_arr)[2], colnames(tmp_arr)[1])
        pie <- plotly::plot_ly(pie_data, labels = ~Region, values = ~value, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste0("Nino Registrations: ",round(value2)),
                showlegend = FALSE)
  }
  ## Use plain ggplot if non interactive
  else {
      # Create a basic bar
      pie = ggplot2::ggplot(tmp_arr, ggplot2::aes(x="", y=value, fill=Region)) + ggplot2::geom_bar(stat="identity", width=1, colour = "white")

      # Convert to pie (polar coordinates) and add labels
      pie = pie + ggplot2::coord_polar("y", start=0) +

                  ggplot2::geom_text(ggplot2::aes(label = c("EU15", "EU2","EU8", "Non EU")), position = ggplot2::position_stack(vjust = 0.5),
                                                                          fontface = "bold", colour = "white") +
                                                                          theme_gov(base_size = 12, base_colour = "gray60") +
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
  if (save==TRUE){

    ggplot2::ggsave("eu_non_eu_pie.png", plot = pie, width = 20, height = 10)
  }
}

  return(pie)

}


#' @title convert_pie_data
#' @description convert data into correct format for plotly pie chart
#' @param data raw data from API
#' @param col1 column 1
#' @param col2 column 2
#' @return data in the correct format for plotly pie chart
#' @examples
#' \dontrun{convert_pie_data(data, columns)}
#' @export
convert_pie_data <- function(data, col1, col2){

  assertthat::is.string(col1)
  assertthat::is.string(col2)
  if (class(data[col1][[1]])=="factor"){
    tmp = as.numeric(levels(data[col1][[1]]))
    tot = sum(as.numeric(levels(data[col1][[1]])))
  } else {
    tmp = data[col1][[1]]
    tot = sum(data[col1][[1]])
  }
  pie_data <- data.frame("Region" = c("EU15", "EU2", "EU8", "non_EU"),
                         'value' = round(100*(tmp/tot)),
                         'value2' = tmp)
  assertthat::are_equal(dim(pie_data), dim(data)+1)
  return (pie_data)
}



#pie <- create_figure_2(data, year_to_date, FALSE, TRUE)
