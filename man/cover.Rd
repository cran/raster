\name{cover}

\docType{methods}

\alias{cover}
\alias{cover,RasterLayer,RasterLayer-method}
\alias{cover,RasterStackBrick,Raster-method}

\title{ Replace NA values with values of other layers }

\description{
Replace \code{NA} values in the first Raster object (\code{x}) with the values of the second (\code{y}), and so forth for additional Rasters. If \code{x} has multiple layers, the subsequent Raster objects should have the same number of layers, or have a single layer only (which will be recycled).
}

\usage{
cover(x, y, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{y}{Raster* object}
  \item{...}{Additional Raster objects, and additional arguments as for \code{\link{writeRaster}}}  
}


\value{
RasterLayer or RasterBrick object
}

\author{Robert J. Hijmans}

\examples{
r1 <- raster(ncols=36, nrows=18)
r1[] <- 1:ncell(r1)
r2 <- setValues(r1, runif(ncell(r1)))
r2[r2<0.5] <- NA
r3 <- cover(r2, r1)
}	

\keyword{methods}
\keyword{spatial}
