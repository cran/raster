\name{getValuesBlock}

\alias{getValuesBlock}
\alias{getValuesBlock,RasterLayer,numeric-method}
\alias{getValuesBlock,RasterLayerSparse,numeric-method}
\alias{getValuesBlock,RasterStack,numeric-method}
\alias{getValuesBlock,RasterBrick,numeric-method}
  
\title{Get a block of raster cell values}

\description{
getValuesBlock returns values for a block (rectangular area) of values of a Raster* object. It will take them from memory if available, else it will read them from disk. 
}

\usage{
getValuesBlock(x, row, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{row}{Numeric. Row number, should be between 1 and nrow(x), or missing in which case all values are returned}
 \item{...}{Additional arguments, see Details}
}
  


\details{
Additional arguments:

\tabular{rll}{  
  \tab  \code{nrows} \tab Integer. How many rows? Default is 1 \cr
  \tab  \code{col}  \tab Integer. Start column. Default is 1\cr
  \tab  \code{ncols}  \tab Integer. How many columns? Default is the number of colums left after the start column \cr
  \tab  \code{lyrs}  \tab Integer (vector). Which layers? Default is all layers (\code{1:nlayers(x)})  \cr
}
}

\value{
vector \code{(x=RasterLayer)} or matrix 
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
