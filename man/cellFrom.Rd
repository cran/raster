\name{cellFrom}

\alias{cellFromRowCol}
\alias{cellFromRowColCombine}
\alias{cellFromRow}
\alias{cellFromCol}
\alias{colFromX}
\alias{rowFromY}
\alias{cellFromXY}
\alias{cellFromLine}
\alias{cellFromPolygon}

  
\title{Get cell, row, or column number}

\description{
Get cell number(s) of a Raster* object from row and/or column numbers.   
Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom.
The last cell number equals the number of cells of the Raster* object.
}

\usage{
cellFromRowCol(object, rownr, colnr)
cellFromRowColCombine(object, rownr, colnr)
cellFromRow(object, rownr)
cellFromCol(object, colnr)
colFromX(object, x)
rowFromY(object, y)
cellFromXY(object, xy)
cellFromLine(object, lns)
cellFromPolygon(object, p, weights=FALSE)
}

\arguments{
  \item{object}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{colnr}{column number; or vector of column numbers}
  \item{rownr}{row number; or vector of row numbers}
  \item{x}{x coordinate(s)}
  \item{y}{y coordinate(s)}
  \item{xy}{matrix of x and y coordinates, or a SpatialPoints or SpatialPointsDataFrame object}
  \item{lns}{SpatialLines object}
  \item{p}{SpatialPolygons object}
  \item{weights}{Logical. If \code{TRUE}, the fraction of each cell that is covered is also returned}
}
  
\details{
  \code{cellFromRowCol} returns the cell numbers obtained for each row / col number pair. In contrast, \code{cellFromRowColCombine} returns the cell numbers obtained by the combination of all row and column numbers supplied as arguments.
}

\value{
row, column or cell number(s). cellFromLine and cellFromPolygon return a list. 
}

\seealso{
\code{\link{xyFromCell}}
}

\author{Robert J. Hijmans }

\examples{
r <- raster(ncols=10, nrows=10)
cellFromRowCol(r, 5, 5)
cellFromRowCol(r, 1:2, 1:2)
cellFromRowColCombine(r, 1:3, 1:2)
cellFromCol(r, 1)
cellFromRow(r, 1)
colFromX(r, 0.5)
rowFromY(r, 0.5)
cellFromXY(r, c(0.5, 0.5))

cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
pols <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), Polygons(list(Polygon(cds2)), 2)))
cellFromPolygon(r, pols)

}

\keyword{spatial}
