\name{zonal}

\alias{zonal}

\title{Zonal statistics}

\description{
Compute zonal statistics. That is, cross-tabulate the values of a Raster* object based on a "zones" RasterLayer. \code{NA} values are removed.
}

\usage{
zonal(x, zones, stat='mean', digits=0, progress) 
}

\arguments{
  \item{x}{Raster* object}
  \item{zones}{RasterLayer object with codes representing zones}
  \item{stat}{The function to be applied. Either as character: 'mean', 'min', 'max', 'sum'; or a function (see Details) }
  \item{digits}{Integer. Number of digits to maintain in 'zones'. By default averaged to an integer (zero digits)}
  \item{progress}{Character. "text", "window", or "" (the default, no progress bar) }   
}

\details{
If \code{stat} is a \code{function}, \code{zonal} will fail (gracefully) for very large RasterLayers
}

\value{
A matrix with a value for each zone (unique value in \code{zones})
}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncols=10, nrows=10)
r[] <- runif(ncell(r)) * 1:ncell(r)
z <- r
z[] <- rep(1:5, each=20)
zonal(r, z, mean)
zonal(r, z, min)
# for big files, use a character value rather than a function
zonal(r, z, 'sum')

# multiple layers
zonal(stack(r, r*10), z, 'sum')

}

\keyword{spatial}
