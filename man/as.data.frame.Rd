\name{as.data.frame}

\alias{as.data.frame}
\alias{as.data.frame,RasterStackBrick-method}
\alias{as.data.frame,RasterLayer-method}
  
\title{Get a data.frame with raster cell values}

\description{
\code{as.matrix} returns all values of a Raster* object as a matrix. For RasterLayers, rows and columns in the matrix represent rows and columns in the RasterLayer object. For other Raster* objects, the matrix returned by \code{as.matrix} has columns for each layer and rows for each cell. 

\code{as.array} returns an array of matrices that are like those returned by \code{as.matrix} for a RasterLayer

If there is insufficient memory to load all values, you can use \code{\link{getValues}} or \code{\link{getValuesBlock}} to read chunks of the file.

}

\usage{
as.data.frame(x, row.names=NULL, optional=FALSE, ...)
}

\arguments{
 \item{x}{ Raster* object }
 \item{row.names}{NULL or a character vector giving the row names for the data frame. Missing values are not allowed.} 
 \item{optional}{ logical. If TRUE, setting row names and converting column names (to syntactic names: see make.names) is optional.}
 \item{...}{ Additional arguments (none).} 
}
  

\value{
data.frame
}

\author{Robert J. Hijmans}

\examples{
r <- raster(ncol=3, nrow=3)
r[] = 1:ncell(r)
as.data.frame(r)
s <- stack(r,r)
as.data.frame(s)
}

\keyword{spatial}
\keyword{methods}
