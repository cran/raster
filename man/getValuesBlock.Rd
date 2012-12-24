\name{getValuesBlock}

\alias{getValuesBlock}
\alias{getValuesBlock,RasterLayer-method}
\alias{getValuesBlock,RasterLayerSparse-method}
\alias{getValuesBlock,RasterStack-method}
\alias{getValuesBlock,RasterBrick-method}
  
\title{Get a block of raster cell values}

\description{
getValuesBlock returns values for a block (rectangular area) of values of a Raster* object. 
}


\usage{
\S4method{getValuesBlock}{RasterLayer}(x, row=1, nrows=1, col=1, ncols=(ncol(x)-col+1), format='')

\S4method{getValuesBlock}{RasterBrick}(x, row=1, nrows=1, col=1, ncols=(ncol(x)-col+1), lyrs)

\S4method{getValuesBlock}{RasterStack}(x, row=1, nrows=1, col=1, ncols=(ncol(x)-col+1), lyrs)
}


\arguments{
 \item{x}{Raster* object}
 \item{row}{positive integer. Row number to start from, should be between 1 and nrow(x)}
 \item{nrows}{postive integer. How many rows? Default is 1}
 \item{col}{postive integer. Column number to start from, should be between 1 and ncol(x)}
 \item{ncols}{postive integer. How many columns? Default is the number of colums left after the start column}
 \item{format}{character. If \code{format='matrix'}, a matrix is returned instead of a vector}
 \item{lyrs}{integer (vector). Which layers? Default is all layers (\code{1:nlayers(x)})}
}


\value{
matrix or vector (if \code{(x=RasterLayer)}, unless \code{format='matrix'})
}

\seealso{ \code{\link{getValues}} }

\examples{
r <- raster(system.file("external/test.grd", package="raster"))
b <- getValuesBlock(r, row=100, nrows=3, col=10, ncols=5)
b 
b <- matrix(b, nrow=3, ncol=5, byrow=TRUE)
b

logo <- brick(system.file("external/rlogo.grd", package="raster"))
getValuesBlock(logo, row=35, nrows=3, col=50, ncols=3, lyrs=2:3)
}

\keyword{spatial}
\keyword{methods}
