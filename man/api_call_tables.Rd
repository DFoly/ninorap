% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_call_countries.R
\name{api_call_tables}
\alias{api_call_tables}
\title{api_call_tables}
\usage{
api_call_tables(start_date = "2017-03-01", end_date, apiKey,
  verbose = FALSE)
}
\arguments{
\item{start_date}{start date of data we want}

\item{end_date}{what date do we want data until}

\item{apiKey}{apiKey for stat-xplore: you will need to create an account to generate a key.}

\item{verbose}{if true, it will return information sent to the Stat-Xplore server.}

\item{frequency}{One of quarter, month, annual}
}
\value{
class containing data and other attributes useful for plotting
}
\description{
Function accesses the Stat-Xplore API and returns country and UK city data for the latest quarter
}
\examples{
\dontrun{eu_non_eu_total("2002-03-01", "2018-12-01", "quarter", 3, TRUE)}
}
