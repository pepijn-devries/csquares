% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_as_sf.R
\name{st_as_sf}
\alias{st_as_sf}
\alias{st_as_sf.csquares}
\alias{st_as_sfc}
\alias{st_as_sfc.csquares}
\title{Create a simple features object from c-squares}
\usage{
st_as_sf.csquares(x, ..., use_geometry = TRUE)

st_as_sfc.csquares(x, ..., use_geometry = TRUE)
}
\arguments{
\item{x}{A \code{vector} of \code{character} strings. Each element should hold a valid
c-square code. \code{x} can also be a \code{data.frame} with a column of c-square codes.
(Note that wildcard characters are not supported)}

\item{...}{Ignored}

\item{use_geometry}{If \code{use_geometry} is \code{TRUE} and \code{x} inherits a spatial feature,
its geometry will be used to cast the object. This is much faster than its alternative
when \code{use_geometry} is \code{FALSE}. In the latter case, the c-square codes are first translated
into explicit spatial information. The latter is more reliable as it does not rely on
the assumption that the geometry of \code{x} corresponds with the csquares codes in the object.
In short: use \code{TRUE} for speed, use \code{FALSE} for reliability.}
}
\value{
In case of \code{st_as_sfc.csquares} a list of geometries (\code{\link[sf:sfc]{sfc}},
(MULTI)POLYGONS) is returned. In case of \code{st_as_sf.csquares} an object of class
(\code{\link[sf:sf]{sf}}) is returned.
}
\description{
Converts a \code{character} string of c-squares in a spatially explicit simple features object
(\code{\link[sf:sf]{sf}}. It can also convert \code{data.frame}s with a column of c-squares codes to
an \code{\link[sf:sf]{sf}} object.
}
\examples{
library(sf)
st_as_sfc(as_csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1"))
st_as_sf(as_csquares("7500:110:3|7500:110:1|1500:110:3|1500:110:1"))
}
\author{
Pepijn de Vries
}
