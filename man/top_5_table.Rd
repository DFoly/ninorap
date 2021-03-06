% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/top_5_table.R
\name{top_5_table}
\alias{top_5_table}
\title{Function accesses the Stat-Xplore API and returns data relating
to Nino Registrations from all countries}
\usage{
top_5_table(end_date, year_to_date, apiKey = NULL)
}
\arguments{
\item{year_to_date}{Last month in quarter, one of: Mar, Jun, Sep, Dec}

\item{apiKey}{apiKey for stat-xplore: you will need to create an account to generate a key.}
}
\value{
Top 5 countries in terms of Nino Registrations.
}
\description{
Function accesses the Stat-Xplore API and returns data relating
to Nino Registrations from all countries
}
