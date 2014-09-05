\name{distanceFromPoints}

\alias{distanceFromPoints}

\title{Distance from points}

\description{
The function calculates the distance from a set of points to all cells of a RasterLayer.
 
The distance unit is in meters if the RasterLayer is not projected (\code{+proj=longlat}) and in map units (typically meters) when it is projected.
}

\usage{
distanceFromPoints(object, xy, filename='', ...) 
}

\arguments{
\item{object}{Raster object}
\item{xy}{matrix of x and y coordinates, or a SpatialPoints* object.}
\item{filename}{character. Optional filename for the output RasterLayer}
\item{...}{Additional arguments as for \code{\link{writeRaster}}}  
}


\value{RasterLayer object}

\seealso{ \code{\link[raster]{distance}},  \code{\link[raster]{gridDistance}},  \code{\link[raster]{pointDistance}}  }




\examples{
r <- raster(ncol=36,nrow=18)
xy = c(0,0)
dist <- distanceFromPoints(r, xy) 
#plot(dist)
}

\keyword{spatial}
