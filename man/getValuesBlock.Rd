\name{getValuesBlock}

\alias{getValuesBlock}
\alias{getValuesBlock,RasterLayer,numeric-method}
\alias{getValuesBlock,RasterStack,numeric-method}
\alias{getValuesBlock,RasterBrick,numeric-method}
\alias{getValuesExtent}
\alias{getValuesExtent,Raster,Extent-method}
  
\title{Get raster cell values}

\description{
getValuesBlock returns values for a block (rectangular area) of values of a Raster* object. It will take them from memory if available, else it will read them from disk. 
}

\usage{
getValuesBlock(x, row, ...)
getValuesExtent(x, e, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{row}{Numeric. Row number, should be between 1 and nrow(x), or missing in which case all values are returned}
 \item{...}{Additional arguments for getValuesBlock, see Details}
 \item{e}{\link[raster]{extent} object} 
}
  

\details{
Additional arguments for readValuesBlock:

\tabular{rll}{  
  \tab  \code{nrows} \tab Integer. How many rows? Default is 1 \cr
  \tab  \code{col}  \tab Integer. Start column. Default is 1\cr
  \tab  \code{ncols}  \tab Integer. How many columns? Default is the number of colums left after the start column \cr
}

There are no additional arguments for readValuesExtent
}

\value{
vector with raster values
}

\author{Robert J. Hijmans}
\examples{
r <- raster(system.file("external/test.grd", package="raster"))
b <- getValuesBlock(r, row=100, nrows=3, col=10, ncols=5)
b
dim(b) = c(3,5)
b
e = extent(180078, 180251, 330247, 330433)
getValuesExtent(r, e)
}

\keyword{spatial}
\keyword{methods}
