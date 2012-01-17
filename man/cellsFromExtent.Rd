\name{cellsFromExtent}

\alias{cellsFromExtent}


\title{Cells from Extent}

\description{
This function returns the cell numbers for a Raster* object that are within a specfied extent (rectangular area), supply either an object of class Extent, or another Raster* object.
}

\usage{
cellsFromExtent(object, extent, expand=FALSE)
}

\arguments{
  \item{object}{A Raster* object}
  \item{extent}{An object of class Extent (which you can create with newExtent(), or another Raster* object )}  
  \item{expand}{Logical. If \code{TRUE}, \code{NA} is returned for (virtual) cells implied by \code{bndbox}, 
      that are outside the RasterLayer (\code{object}). If \code{FALSE}, only cell numbers for the area where \code{object} and \code{bndbox}
      overlap are returned (see \link[raster]{intersect}) }
}

\value{
a vector of cell numbers 
}  
  
\author{Robert J. Hijmans}

\seealso{  \code{\link[raster]{extent}}, \code{\link[raster]{polygonFromExtent}}, \code{\link{cellFromXY}}
 }

\examples{
r <- raster()
bb <- extent(-5, 5, -5, 5)
cells <- cellsFromExtent(r, bb)
r <- crop(r, bb)
r[] <- cells
}

\keyword{spatial}
