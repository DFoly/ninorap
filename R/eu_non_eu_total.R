#' Function accesses the Stat-Xplore API and returns data relating
#' to Nino Registrations from the EU and outside the EU. This data is used
#' to construct a time series graph of these registrations over time.
#' @param start_date start date of data we want
#' @param end_date what date do we want data until
#' @param frequency One of quarter, month, annual
#' @param apiKey apiKey for stat-xplore: you will need to create an account to generate a key.
#' @param verbose if true, it will return information sent to the Stat-Xplore server.
#' @return class containing data and other attributes useful for plotting
#' @examples \dontrun{eu_non_eu_total("2002-03-01", "2018-12-01", "quarter", 3, TRUE)}
#' @source \url{https://stat-xplore.dwp.gov.uk/webapi/jsf/login.xhtml}
#' @export


eu_non_eu_total <- function(start_date, end_date, frequency, apiKey, verbose = FALSE) {

    apiKey <- "65794a30655841694f694a4b563151694c434a68624763694f694a49557a49314e694a392e65794a7063334d694f694a7a644849756333526c6247786863694973496e4e3159694936496d5268626d35355a6a4532514768766447316861577775593239744969776961574630496a6f784e54517a4f5445354f4455304c434a68645751694f694a7a6448497562325268496e302e70496474346b5763677546564d42486b6773484f306a6d5f536d556b6a33586e574946527041516f794f6f"

    out <- tryCatch(
      expr = {

    # Generate API call for paritcular dates
    date_list <- generate_date_sequence(start_date, end_date, frequency)
    dates <- date_list[[1]]
    length_dates <- date_list[[2]]


    # Generate JSON date sequence
    num_quarters = seq(from=1, to=length_dates, by=1)
    quarters <- lapply(num_quarters, function(x) paste('["str:value:NINO:f_NINO:QTR:c_QTR:',x,'"',']' ,sep=""))
    quarters_final <- paste(quarters, collapse=",")


    # need to convert to string
    body <- paste0('{"database":"str:database:NINO","measures":["str:count:NINO:f_NINO"],"recodes":{"str:field:NINO:f_NINO:NEWNAT":{"map":[["str:value:NINO:f_NINO:NEWNAT:C_NINO_WORLDAREA:1"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_SUBGROUP:1"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_SUBGROUP:2"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_SUBGROUP:3"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_WORLDAREA:2"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_WORLDAREA:3"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_WORLDAREA:4"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_WORLDAREA:9"]],"total":true},"str:field:NINO:f_NINO:QTR":{"map":[',quarters_final,'],"total":true}},"dimensions":[["str:field:NINO:f_NINO:QTR"],["str:field:NINO:f_NINO:NEWNAT"]]}', sep='')

    if (verbose==TRUE) {
    request <- httr::POST("https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table", httr::add_headers(apiKey = apiKey),
                       body = body,
                       encode = "json", httr::verbose())
    }
    request <- httr::POST("https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table", httr::add_headers(apiKey = apiKey),
                          body = body,
                          encode = "json")

    get2json<- httr::content(request, as = "parsed")
    parse2json<- jsonlite::toJSON(get2json, pretty=TRUE) # returns raw data

    parse_df <- as.data.frame(t(sapply(parse2json, jsonlite::fromJSON)))

    title <- parse_df$database[[1]]$label

    # Extract dates
    data_values <- parse_df$cubes[[1]][[1]][[1]]
    date_labels <- unlist(parse_df$fields[[1]][[4]][[1]]$labels)
    nrows = dim(data_values)[1]
    ncols =  dim(data_values)[2]
    date_labels <- date_labels[1:nrows-1]

    # destination labels
    origin <- unlist(parse_df$fields[[1]][[4]][[2]]$labels)

    # create data frame
    df <- as.data.frame(data_values)

    colnames(df) <-  gsub(" ", "_", origin)
    colnames(df) <-  gsub("-", "_", colnames(df))

    #create non-eu data
    df <- df %>% dplyr::mutate(non_eu = Total - European_Union)

    totals <- df[nrows,]
    df <- df[1:nrows-1,]


    #Create class
    x = structure(
      list(
        df = df,
        #df_year_to_date_sum =
        title = title,
        colnames = colnames(df),
        dates = dates,
        date_labels = date_labels
      ),
      class = "eu_non_eu_data")

    return(x)
    ##
      },
    warning = function() {

      w <- warnings()
      warning('Warning produced running in eu_non_eu_total():', w)

    },
    error = function(e)  {

      stop('Error produced running eu_non_eu_total():', e)

    },
    finally = {}
    )
}


generate_date_sequence <- function(start_date, end_date, frequency) {
  dates <- seq(lubridate::ymd(start_date), lubridate::ymd(end_date), by = frequency)
  N = length(dates)

  return(list(dates, N))
}






#test_data <- eu_non_eu_total("2002-03-01", "2010-12-01", "quarter")
#df_struct <- eu_non_eu_total("2002-03-01", "2018-12-01", "quarter")
# Year to date for quarterly data
# df_new <- zoo::rollapply(df, 4, sum, by.column=TRUE)
