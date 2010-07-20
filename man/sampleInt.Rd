\name{SampleInt}

\alias{sampleInt}

\title{Sample integer values}

\description{
Take a random sample from a range of Integer values between 1 and n. Its purpose is similar to that of \code{\link[base]{sample.int}}, but that function fails when n is very large.
}

\usage{
sampleInt(n, size, replace=FALSE)
}

\arguments{
  \item{n}{a poistive number (integer); the number of items to choose from }
  \item{size}{a non-negative integer; the number of items to choose.}  
  \item{replace}{logical. Should sampling be with replacement?}
 }


\value{vector of integer numbers}

\author{Robert J. Hijmans}

\examples{
  sampleInt(1e+12, 10)
  
# this may fail:
#  sample.int(1e+12, 10)
#  sample.int(1e+9, 10)
}


\keyword{spatial}
