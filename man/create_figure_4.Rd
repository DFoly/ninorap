% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_figure_4.R
\name{create_figure_4}
\alias{create_figure_4}
\title{Create Figure 4}
\usage{
create_figure_4(data, year_to_date, labels, save = TRUE,
  interactive = FALSE)
}
\arguments{
\item{data}{raw data from API call}

\item{year_to_date}{Last month in quarter, one of: Mar, Jun, Sep, Dec}

\item{labels}{labels from API call must be in colnames of data}

\item{save}{whether or not to save figure as png Default is TRUE
use data$colnames to see available choices}
}
\value{
plot of class ggplot
}
\description{
This function produces Figure 4 from the publication. Registration
by world area current year vs the previous year.
}
\examples{
\dontrun{create_figure_3(data, year_to_date)}
}
