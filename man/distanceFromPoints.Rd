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
\item{object}{RasterLayer object}
\item{xy}{Matrix of x and y coordinates, or a SpatialPoints* object.}
\item{filename}{Filename for the output RasterLayer}
\item{...}{Additional arguments. See Details.}  
}

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}


\value{A RasterLayer object}

\seealso{ \code{\link[raster]{distance}},  \code{\link[raster]{gridDistance}},  \code{\link[raster]{pointDistance}}  }

\author{Robert J. Hijmans }


\examples{
r <- raster(ncol=36,nrow=18)
xy = c(0,0)
dist <- distanceFromPoints(r, xy) 
#plot(dist)
}

\keyword{spatial}
