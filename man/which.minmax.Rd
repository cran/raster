\name{which.min}

\docType{methods}

\alias{which.min}
\alias{which.max}
\alias{which.min,RasterLayer-method}
\alias{which.max,RasterLayer-method}
\alias{which.min,RasterStackBrick-method}
\alias{which.max,RasterStackBrick-method}

\title{Where is the min or max value?}

\description{Which cells have the minumum / maximum value (for a RasterLayer), or which layer has the minimum/maximum value (for a RasterStack or RasterBrick)?}

\usage{
which.min(x)
which.max(x)
}

\arguments{
  \item{x}{Raster* object}
}

\value{
vector of cell numbers (if \code{x} is a RasterLayer) or (if \code{x} is a RasterStack or RasterBrick) a RasterLayer giving the number of the first layer with the minimum or maximum value for a cell
}

\seealso{ \code{\link{Which}}}


\examples{
\dontrun{
b <- brick(system.file("external/rlogo.grd", package="raster")) 

r <- which.min(b)

i <- which.min(b[[3]])
xy <- xyFromCell(b, i)
plot(b[[3]])
points(xy)
}
}

\keyword{spatial}
