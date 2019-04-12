#' @title Create Figure 3
#' @description This function produces Figure 3 from the publication.
#' Plots total number of Nino Registrations from EU2, EU8 and EU15.
#' @param data raw data from API call
#' @param year_to_date Last month in quarter, one of: Mar, Jun, Sep, Dec
#' @param year_end_filter raw data from API call
#' @param labels labels from API call must be in colnames of data
#' use data$colnames to see available choices
#' @return plot of class ggplot
#' @examples
#' \dontrun{create_figure_3(data, year_to_date)}
#' @export

create_figure_3 <- function(data, year_to_date, year_end_filter, labels, save=TRUE){


  out <- tryCatch(
    expr = {

      data <- transform_data_figure_3(data, year_to_date, year_end_filter, labels)
      nrows = dim(data)[1]
      ncol = dim(data)[2]
      # Scale data for plotting
      data[,ncol] <- data[,ncol]/1000

      ####################
      # Create Plot
      ###################
      x_len = length(data$quarter_dates)/ncol(data)
      unique_countries <- unique(data$variable)
      unique_quarter_dates <- unique(data$quarter_dates)

      group.colors <- unique_countries
      group.colors <- c(EU2 = "#2B8CC4", EU8 = "#2E358B", EU15 ="grey60")
      g <- ggplot2::ggplot(data, ggplot2::aes(x = quarter_dates, y =value,
                                              group = variable, colour = variable,
                                              linetype = variable)) +
        ggplot2::geom_path(size = 1.5) + theme_gov() +
        ggplot2::scale_color_manual(values=group.colors)

      #####################################################################
      # Annotations: to do allow user to input important events and dates.
      #####################################################################
      text_for_plot <- c("2004-EU8 joined the EU", "2010-Eurozone debt impact",
                "2014-Lifting of transitional controls for EU2",
                "2016-EU Referendum")
      important_dates <- c("Dec 04", "Dec 10", "Dec 14", "Dec 16")
      indices <- c()
      for (i in 1:length(important_dates)) {
        print(i)
        print(important_dates[i])
        indices[i] = which(important_dates[i] == unique_quarter_dates)
      }

      # The following calulations all help with the positioning of labels
      latest_date <- data$quarter_dates[[nrows]]
      start_index <- 1
      end_index <- length(important_dates)
      y_max <- max(data$value)
      y_min <- min(data$value)*10

      EU8_latest <- data[(data$quarter_dates == latest_date) & (data$variable == "EU8"),]$value
      EU2_latest <- data[(data$quarter_dates == latest_date) & (data$variable == "EU2"),]$value
      EU15_latest <- data[(data$quarter_dates == latest_date) & (data$variable == "EU15"),]$value

      # check growth rates to ADJUST LABELS at end of graph
      EU15_shift = ifelse(country_latest[1] - data$value[row_indices[1]]>0,1,-1)
      EU8_shift =  ifelse(country_latest[2] - data$value[row_indices[2]]>0,1,-1)
      EU2_shift =  ifelse(country_latest[3] - data$value[row_indices[3]]>0,1,-1)

      EU15_min = min(data[data$variable == "EU15",]$value)
      EU8_min = min(data[data$variable == "EU8",]$value)
      EU2_min = min(data[data$variable == "EU2",]$value)

      country_latest <- c()
      row_indices <- c()
      counter = 1
      for (country in unique_countries) {
        query <- data[(data$quarter_dates == latest_date) & (data$variable == country),]
        country_latest[counter] = query$value
        row_indices[counter] = as.numeric(rownames(query))
        counter = counter + 1
      }


      #annotate important events
      g + ggplot2::annotate("text", x = indices, y = y_max-y_min, label = stringr::str_wrap(text_for_plot, 2)) +

          # Annotate origin
          ggplot2::annotate("text", x =x_len, y = country_latest[1]+EU15_shift*(EU8_min), label = unique_countries[1],
                            color = group.colors[unique_countries[1]]) +
          ggplot2::annotate("text", x =x_len, y = country_latest[2]+EU8_shift*(EU8_min), label = unique_countries[2],
                          color = group.colors[unique_countries[2]]) +
          ggplot2::annotate("text", x =x_len, y = country_latest[3]+EU2_shift*(EU8_min), label = unique_countries[3],
                          color = group.colors[unique_countries[3]])




    if (save==TRUE){
      ggplot2::ggsave("nino_registrations_EU.png", plot = g, width = 20, height = 10)
    }
    return(g)
  ##
    },
  warning = function() {

    w <- warnings()
    warning('Warning produced running in create_figure_3():', w)

  },
  error = function(e)  {

    stop('Error produced running  create_figure_3():', e)

  },
  finally = {}
  )
}

#create_figure_3(data, year_to_date, year_end_filter, labels)
