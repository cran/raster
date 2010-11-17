\name{as.matrix}

\alias{as.matrix}
\alias{as.matrix,Raster-method}
\alias{as.matrix,RasterLayer-method}
  
\title{Get a matrix with raster cell values}

\description{
Return all values of a Raster* object as a matrix. For RasterLayer objects this is a wrapper around \code{\link{getValues}(x, format='matrix'} and rows and columns in the matrix represent rows and columns in the RasterLayer object. For other Raster* objects a warpper around \code{\link{getValues}(x)}, and each column represents a layer. If there is insufficient memory to load all values, you can use \code{\link{getValues}} or \code{\link{getValuesBlock}} to read chunks of the file.

}

\usage{
as.matrix(x, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{...}{Additional arguments. None implemented}
}
  

\value{
matrix 
}

\author{Robert J. Hijmans}

\examples{
r <- raster(system.file("external/test.grd", package="raster"))
m <- as.matrix(r)
dim(m)
m[50:60, 25:35]
}

\keyword{spatial}
\keyword{methods}
