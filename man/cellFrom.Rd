\name{cellFrom}

\alias{cellFromRowCol}
\alias{cellFromRowColCombine}
\alias{cellFromRow}
\alias{cellFromCol}
\alias{colFromX}
\alias{rowFromY}
\alias{cellFromXY}

  
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
}

\arguments{
  \item{object}{Raster* object (or a SpatialPixels* or SpatialGrid* object)}
  \item{colnr}{column number; or vector of column numbers}
  \item{rownr}{row number; or vector of row numbers}
  \item{x}{x coordinate(s)}
  \item{y}{y coordinate(s)}
  \item{xy}{matrix of x and y coordinates, or a SpatialPoints or SpatialPointsDataFrame object}

}
  
\details{
  In \code{cellFromRowCol}, \code{rownr} and  \code{colnr} should have the same length.
  This is not the case for \code{cellFromRowColCombine}. This function returns the cell numbers obtained by 
  the combination of row and column numbers.
}

\value{
row, column or cell number(s) 
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

}

\keyword{spatial}
