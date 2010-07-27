\name{rasterFromCells}

\alias{rasterFromCells}

\title{Subset a raster by cell numbers}

\description{
This function returns a new raster based on an existing raster and cell numbers for that raster.
The new raster is cropped to the cellnumbers provided, and has values that are the cell numbers of the original raster.
}

\usage{
rasterFromCells(object, cells)
}

\arguments{
  \item{object}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{cells}{vector of cell numbers}
}
  
\details{
Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom
The last cell number equals the number of cells of the Raster* object.
}

\value{
A RasterLayer
}

\seealso{ \code{\link[raster]{rowFromCell}} }

\author{Robert J. Hijmans }

\examples{
r <- raster(ncols=100, nrows=100)
cells <- c(3:5, 210)
r <- rasterFromCells(r, cells)
cbind(1:ncell(r), values(r))
}

\keyword{spatial}
