\name{modal}

\alias{modal}
\alias{modal,ANY-method}
\alias{modal,Raster-method}

\title{modal value}

\description{
Compute the mode for a vector of numbers, or across raster layers. The mode, or modal value, is the most frequent value in a set of values. 
}

\usage{
\S4method{modal}{ANY}(x, ..., ties='random', na.rm = FALSE)

\S4method{modal}{Raster}(x, ..., ties='random', na.rm = FALSE)
}

\arguments{
  \item{x}{A vector of numbers (typically integers for modal), or a Raster* object}
  \item{...}{additional (vectors of) numbers, or additional Raster* objects}
  \item{ties}{character. Indicates how to treat ties. Either 'random', 'lowest', 'highest', or 'NA'}
  \item{na.rm}{Remove (ignore) NA values}
}

\value{
vector or RasterLayer
}

\author{Robert J. Hijmans}

\examples{
data <- c(0,1,2,3,3,3,3,4,4,4,5,5,6,7,7,8,9,NA)
modal(data, na.rm=TRUE)
}

\keyword{univar}
\keyword{math}
