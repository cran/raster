\name{misc stats}

\alias{modal}
\alias{modal,ANY-method}
\alias{modal,Raster-method}


\alias{cv}
\alias{cv,ANY-method}
\alias{cv,Raster-method}

\title{Miscellaneous statistical functions}

\description{
  mode and coefficient of variation
}

\usage{
modal(x, ..., ties='random', na.rm = FALSE)
cv(x, ..., aszero=FALSE, na.rm = FALSE)
}

\arguments{
  \item{x}{A vector of numbers (typically integers for modal)}
  \item{...}{additional (vectors of) numbers}
  \item{ties}{character. Indicates how to treat ties. Either 'random', 'lowest', 'highest', or 'NA'}
  \item{na.rm}{Remove (ignore) NA values}
  \item{aszero}{logical. If \code{TRUE}, a zero is returned (rather than an NA) if the cv of single value is computed}
}

\value{
  modal returns the mode, or modal (most frequent) value of a vector of values, ties are broken at random.
  cv returns the coefficient of variation (expressed as a percentage). If there is only a single value, \code{sd} is \code{NA}
  and \code{cv} returns \code{NA} if \code{aszero=FALSE} (the default). However, if (\code{aszero=TRUE}), \code{cv} returns \code{0}.
}

\author{Robert J. Hijmans}

\examples{
data <- c(0,1,2,3,3,3,3,4,4,4,5,5,6,7,7,8,9,NA)
modal(data, na.rm=TRUE)
cv(data, na.rm=TRUE)
}

\keyword{univar}
\keyword{math}
