% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_pm_data.R
\name{add_pm}
\alias{add_pm}
\title{add PM2.5 concentrations to geocoded data based on h3 geohash or lat/lon coords}
\usage{
add_pm(d, type = "coords", verbose = FALSE, ...)
}
\arguments{
\item{d}{dataframe with columns called "lat", "lon", "start_date" and "end_date"}

\item{type}{either "coords" (if d contains lat/lon) or "h3" (if d contains
resolution 8 h3 ids)}

\item{verbose}{if TRUE a statement is printed to the console telling the user
which chunk file is currently being processed. Defaults to FALSE.}

\item{...}{arguments passed to \code{\link[s3]{s3_get_files}}}
}
\value{
the input dataframe, expanded to include one row per day between the given "start_date"
and "end_date", with appended columns for h3_3 (resolution 3), h3 (resolution 8),
year, pm_pred, and pm_se.
}
\description{
add PM2.5 concentrations to geocoded data based on h3 geohash or lat/lon coords
}
\examples{
d <- tibble::tribble(
  ~id, ~lat, ~lon, ~start_date, ~end_date,
  "55000100280", 39.2, -84.6, "2008-09-09", "2008-09-11",
  "55000100281", 39.2, -84.6, "2007-08-05", "2007-08-08",
  "55000100282", 39.2, -84.6, "2015-08-31", "2015-09-02"
)

add_pm(d)
}
