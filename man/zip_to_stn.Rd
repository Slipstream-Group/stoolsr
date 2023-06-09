% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_weather.R
\name{zip_to_stn}
\alias{zip_to_stn}
\title{Find Nearest NOAA ISD Station for a Zip Code}
\usage{
zip_to_stn(
  zip5,
  states = NULL,
  begin_date = NULL,
  end_date = NULL,
  max_dist = 100,
  return_all = FALSE,
  exclude_ids = NULL
)
}
\arguments{
\item{zip5}{vector of valid 5-digit zip codes}

\item{states}{optional character vector of states. Defaults to zip5 state.}

\item{begin_date}{optional begin date for needed weather data (YYYY-MM-DD)}

\item{end_date}{optional end date for needed weather data (YYYY-MM-DD)}

\item{max_dist}{maximum allowable distance for matching a station}

\item{return_all}{return all stations rather than only nearest}

\item{exclude_ids}{station_ids (use isd_stns() to get ids) to exclude from matching}
}
\value{
data frame
}
\description{
Find Nearest NOAA ISD Station for a Zip Code
}
\examples{
zip_to_stn(zip5 = c("53715", "53235", "54255"), states = "WI", end_date = Sys.Date()-2)

}
