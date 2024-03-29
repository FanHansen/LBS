% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lat_lng_to_add.R
\name{lat_lng_to_address_all}
\alias{lat_lng_to_address_all}
\title{Transform multiple longitude and latitude to address}
\usage{
lat_lng_to_address_all(dat = NULL, lng = NULL, lat = NULL,
  ID = NULL, map = "baidu", key = NULL, parallel = FALSE,
  coordtype = "wgs84ll")
}
\arguments{
\item{dat}{data.frame contains Longitude and Latitudes.variables.}

\item{lng}{Name of Longitude variable of dat, or a list of Longitudes.}

\item{lat}{Name of Latitude variable of dat, or a list of Latitudes.}

\item{ID}{The name of ID of observations. Default is NULL.}

\item{map}{Which map API, 'baidu' and 'ampap' are available.}

\item{key}{Key of map. Apply from \url{https://lbs.amap.com/api/webservice/guide/api/georegeo/}
or \url{https://developer.baidu.com/map/android-mobile-apply-key.htm}}

\item{parallel}{Parallel computing option.Default is FALSE.}

\item{coordtype}{coordtype of longitude and latitude. Default is wgs8411.}
}
\value{
A data.frame with formatted address.
}
\description{
\code{lat_lng_to_address_all} is used for transforming multiple longitude and latitude to address.
}
\examples{
lat_lng_to_address_all(lat = c(30.00,40.00), lng = c(120,110),parallel = FALSE)
}
