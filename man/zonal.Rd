\name{zonal}

\alias{zonal}

\title{Zonal statistics}

\description{
Compute zonal statistics. That is, cross-tabulate the values of a RasterLayer based on a "zones" RasterLayer. 
}

\usage{
zonal(raster, zones, stat='mean', na.rm=TRUE, progress) 
}

\arguments{
  \item{raster}{A RasterLayer object}
  \item{zones}{A RasterLayer object; with codes representing zones that are treated as integer values}
  \item{stat}{The function to be applied. Either as character: 'mean', 'min', 'max', 'sum'; or a function (see Details) }
  \item{na.rm}{Logical. If \code{TRUE}, NA will be removed from the computations and the result will only be NA if all cells in a zone are NA}
  \item{progress}{Character. "text", "window", or "" (the default, no progress bar) }   
}

\details{
If \code{stat} is a \code{function}, \code{zonal} will fail (gracefully) for very large RasterLayers
}

\value{
A data frame with a value for each zone
}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=10, nrows=10)
r[] <- runif(ncell(r)) * 1:ncell(r)
z <- r
z[] <- rep(1:5, each=20)
zonal(r, z, 'mean')
zonal(r, z, 'min')
zonal(r, z, 'sum')
# using a function, rather than a character value
zonal(r, z, sum)
}

\keyword{spatial}
