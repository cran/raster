\name{freq}

\docType{methods}

\alias{freq}
\alias{freq,RasterLayer-method}

\title{Frequency table}

\description{
Frequency table of the values of a RasterLayer.  
}

\usage{
freq(x, ...)
}

\arguments{
  \item{x}{A RasterLayer object}
  \item{...}{Additional arguments. See under Details}   
}

\value{
A matrix
}

\section{Methods}{
\describe{
A full call as implemented here:

\code{freq(x, digits=0, progress, ...)}

the \code{digits} argument is used passed to \code{\link[base]{round}}

with \code{progress} a progress bar can be specied ('text', 'tcltk', or "windows")
}}


\seealso{ \code{\link[raster]{count}}, \code{\link[raster]{crosstab} } and \code{\link[raster]{zonal} } }

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r))
r <- r * r * r * 10
freq(r)
}

\keyword{spatial}
\keyword{univar}
