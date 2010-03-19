\name{values}

\docType{methods}

\alias{values}
\alias{values,RasterLayer-method}
\alias{values,RasterBrick-method}
  
\title{Get raster values}

\description{
This function returns the raster cell values for a RasterLayer or RasterBrick object that are currently in memory (typically after read* has been used).
}

\usage{
values(x, ...)
}

\arguments{
 \item{x}{RasterLayer or RasterBrick object}
 \item{...}{Additional arguments. These are only relevant if \code{x} is a RasterLayer object. With the argument format='matrix' the function returns a matrix in stead of a vector; and in that case a further argument 'names=TRUE', will add row and column names to that matrix}
}
  

\details{
In most cases it is better to use \code{\link[raster]{getValues}} rather than \code{values}. \code{values} is typically used in functions after a read* function has been used and it will return all the values that happen to be in memory at that point, or fail if there are no values in memory. In contrast \code{getValues} will read values from disk if necessary. 
}

\value{
a vector or matrix 
}

\seealso{
\code{\link[raster]{getValues}}, \code{\link[raster]{read}}
}


\author{Robert J. Hijmans}
\examples{
r <- raster(ncol=10, nrow=10)
r <- setValues(r, 1:ncell(r))
values(r)
r <- raster(system.file("external/test.grd", package="raster"))
r <- readBlock(r, startrow=100, nrows=3, startcol=5, ncolumns=5)
values(r, format='matrix', names=TRUE)
}

\keyword{spatial}
\keyword{methods}
