\name{as.matrix}

\alias{as.matrix}
\alias{as.array}
\alias{as.array,RasterStackBrick-method}
\alias{as.array,RasterLayer-method}
\alias{as.matrix,RasterStackBrick-method}
\alias{as.matrix,RasterLayer-method}
  
\title{Get a matrix with raster cell values}

\description{
\code{as.matrix} returns all values of a Raster* object as a matrix. For RasterLayers, rows and columns in the matrix represent rows and columns in the RasterLayer object. For other Raster* objects, the matrix returned by \code{as.matrix} has columns for each layer and rows for each cell. 

\code{as.array} returns an array of matrices that are like those returned by \code{as.matrix} for a RasterLayer

If there is insufficient memory to load all values, you can use \code{\link{getValues}} or \code{\link{getValuesBlock}} to read chunks of the file.

}

\usage{
as.matrix(x, ...)
as.array(x, ...)
}

\arguments{
 \item{x}{ Raster* object }
 \item{...}{ Additional arguments. 
 
	\code{maxpixels} Integer. To regularly subsample very large objects 
	
   \code{transpose} Logical. Transpose the data? (for as.array only)

}}
  

\value{
matrix or array
}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncol=3, nrow=3)
r[] = 1:ncell(r)
as.matrix(r)
s <- stack(r,r)
as.array(s)
}

\keyword{spatial}
\keyword{methods}
