\name{Gain and offset}

\alias{gain}
\alias{off}
\alias{gain<-}
\alias{off<-}

\title{Gain and offset of values on file}


\description{
These functions can be used to get or set the gain and offset parameters used to transform values when reading from a file (currently these are ignored when writing a file). The gain and offset parameters are applied to the raw values using the formula below: 

\code{value <- value * gain + offset}

The default value for gain is 1 and for offset is 0. 'gain' is sometimes referred to as 'scale'.

Note that setting gain and/or offset only affects reading of values from a file, it does not change values that are already (or only) in memory.
}

\usage{
gain(x)
gain(x) <- value
off(x)
off(x) <- value
}

\arguments{
  \item{x}{Raster* object}
  \item{value}{Single numeric value }
}

\value{
Raster* object or numeric value(s)
}

\author{Robert J. Hijmans}

\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
gain(r)
off(r)
r[1505:1510]
gain(r) <- 10
off(r) <- 5
r[1505:1510]
}

\keyword{ spatial }
\keyword{ methods }
