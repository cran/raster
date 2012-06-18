\name{count}

\alias{count,Raster-method}
\alias{count}

\title{Count}

\description{
Count the frequency of a single value in a Raster object.  
}

\usage{
\S4method{count}{Raster}(x, value, digits=0, progress='', ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{value}{The value to be counted, can be a number, a logical value, or NA}  
  \item{digits}{Non-negative integer. The number of digits for rounding the values, default is 0, i.e. numbers are compared as integers}
  \item{progress}{Character to specify a progres bar. Choose from 'text', 'window', or '' (the default, no progress bar)}
  \item{...}{Additional arguments. None implemented}
}

\value{
numeric
}

\seealso{ \code{\link{freq}} }

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r))
r <- r * r * r * 10
count(r, 5)
}

\keyword{univar}
