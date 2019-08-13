#' Function accesses the Stat-Xplore API and returns data relating
#' to Nino Registrations from all countries
#' @param end_date
#' @param year_to_date Last month in quarter, one of: Mar, Jun, Sep, Dec
#' @param apiKey apiKey for stat-xplore: you will need to create an account to generate a key.
#' @return Top 5 countries in terms of Nino Registrations.
#' @export


top_5_table <- function(end_date, year_to_date, apiKey = NULL) {
    start_date = "2017-03-01"

    assertthat::assert_that(year_to_date %in% c("Mar", "Jun", "Sep", "Dec"))

    if (is.null(apiKey)) {

        apiKey <- "65794a30655841694f694a4b563151694c434a68624763694f694a49557a49314e694a392e65794a7063334d694f694a7a644849756333526c6247786863694973496e4e3159694936496d5268626d35355a6a4532514768766447316861577775593239744969776961574630496a6f784e54517a4f5445354f4455304c434a68645751694f694a7a6448497562325268496e302e70496474346b5763677546564d42486b6773484f306a6d5f536d556b6a33586e574946527041516f794f6f"
    }

    if (missing(end_date)) {

          end_date = "2018-12-01"
        }

      data <- eu_non_eu_total(start_date, end_date, 'quarter', TRUE, FALSE)
      data <- year_to_date_sum(data, year_to_date)

      # filter by year_to_date
      year_end_data <- dplyr::filter(data[[1]], grepl(year_to_date, data[[1]]$quarter_dates))

      # get rid of first column for melting data
      year_end_data <- year_end_data[,-1]
      N = dim(year_end_data[1])[1]
      ncol =  dim(year_end_data)[2]


      year_end_data <- year_end_data[N,1:(ncol-1)]

      # Melt countries into one column and sort by value
      reshaped_data <- reshape2::melt(year_end_data, id = 'quarter_dates') %>% dplyr::arrange(desc(value))
      top_5 <- reshaped_data[1:5, ]

      return(top_5)

}

#top_5_table("2018-12-01","Dec", apiKey = "aksjhdkajshd")
