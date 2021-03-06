% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_data_figure_3.R
\name{transform_data_figure_3}
\alias{transform_data_figure_3}
\title{Transform data figure 3}
\usage{
transform_data_figure_3(data, year_to_date, year_end_filter, labels)
}
\arguments{
\item{data}{raw data returned from the API call}

\item{year_to_date}{one of Mar, Jun, Sep, Dec}

\item{year_end_filter}{column to extract: e.g. quarter_dates}

\item{labels}{list of columns we want to extract}
}
\value{
filtered data for plotting
}
\description{
Takes in data returned from API call and maninulates into correct form for figure 3
}
\examples{
\dontrun{extract_year_end_data(data, date_filter, labels)}
}
