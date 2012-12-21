
\name{getValuesFocal}

\alias{getValuesFocal}
\alias{getValuesFocal,Raster-method}
\alias{getValuesFocal,Raster,missing,missing,numeric-method}
\alias{getValuesFocal,Raster,numeric,numeric,numeric-method}
  
\title{Get focal raster cell values}

\description{
This function returns all values or a row of values for a Raster* object. It will take them from memory if available, else it will read them from disk. 
Function \code{values} is a shorthand version of getValues.
}

\usage{
\S4method{getValuesFocal}{Raster}(x, row, nrows, ngb, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{row}{Numeric. Row number, should be between 1 and nrow(x). Can be omitted to get all rows}
 \item{nrows}{Numeric. Number of rows, should be an integer > 0 & < row+nrow(x). Should be omitted if \code{row} is omitted}
 \item{ngb}{Neighborhood size. Either a single integer or a vector of two integers \code{c(nrow, ncol)}}
  \item{...}{Additional arguments. One implemented: \code{names} (Boolean). If \code{TRUE}, the matrix returned has row and column names}
}


\value{
If \code{x} has a single layer, a matrix with one row for each focal cell, and one column for each neighborhood cell around it.
If \code{x} has multiple layers, a list of such matrices (one list element / matrix for each layer)
}

\seealso{ \code{\link{getValues}, \link{focal}} }


\examples{
r <- raster(nr=5, nc=5, crs='+proj=utm +zone=12')
r[] <- 1:25
as.matrix(r)
getValuesFocal(r, row=1, nrows=3, ngb=3, names=TRUE)
}

\keyword{spatial}
\keyword{methods}
