\name{xyFromCell}

\alias{xFromCol}
\alias{yFromRow}
\alias{xyFromCell}
\alias{xFromCell}
\alias{yFromCell}

  
\title{Coordinates from a row, column or cell number}

\description{
These functions get coordinates of the center of raster cells for a row, column, or cell number of a Raster* object.
}

\usage{
xFromCol(object, colnr)
yFromRow(object, rownr)
xyFromCell(object, cell, spatial=FALSE)
xFromCell(object, cell)
yFromCell(object, cell)
}


\arguments{
  \item{object}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{cell}{cell number(s)}
  \item{colnr}{column number; or vector of column numbers}
  \item{rownr}{row number; or vector of row numbers}
  \item{spatial}{return a SpatialPoints object (sp package) instead of a matrix}
}
  
\details{
Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom.
The last cell number equals the number of cells of the Raster* object.
}

\value{
  xFromCol, yFromCol, xFromCell, yFromCell: vector of x or y coordinate(s) 
  xyFromCell: matrix(x,y) of pairs of coordinates 
}
\author{Robert J. Hijmans }

\seealso{
\code{\link{cellFromXY}}
}

\examples{
#using a new default raster (1 degree global)
r <- raster()
xFromCol(r, c(1, 120, 180))
yFromRow(r, 90)
xyFromCell(r, 10000)
xyFromCell(r, c(0, 1, 32581, ncell(r), ncell(r)+1))

#using a file from disk
r <- raster(system.file("external/test.grd", package="raster"))
r
cellFromXY(r, c(180000, 330000))
#xy for corners of a raster:
xyFromCell(r, c(1, ncol(r), ncell(r)-ncol(r)+1, ncell(r)))
}

\keyword{spatial}
