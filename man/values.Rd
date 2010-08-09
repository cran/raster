\name{values}

\docType{methods}

\alias{values}
\alias{values,RasterLayer-method}
  
\title{Get raster values}

\description{
This function will be removed shortly. Use \code{\link[raster]{getValues}} instead. 

It returns the raster cell values for a RasterLayer or RasterBrick object that are currently in memory (typically after read* has been used). 
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
\code{\link[raster]{getValues}}
}


\author{Robert J. Hijmans}

\keyword{spatial}
\keyword{methods}
