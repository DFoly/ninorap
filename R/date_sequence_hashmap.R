#' Creates hashmap containing sequence of numbers with keys as the dates
#' This helps generate the apicall to match the correct time periods
#' chosen with the quarter number used by the api call.
#' @export

date_sequence = seq(as.Date("2002-03-01"), as.Date("2025-12-01"), by = "quarter")
int_sequence = seq(1,length(date_sequence), by = 1)

date_hashmap <- int_sequence

names(date_hashmap) <- date_sequence

