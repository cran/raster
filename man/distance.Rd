\name{distance}

\alias{distance}
\alias{distance,RasterLayer-method}

\title{Distance}

\description{
Calculate the distance, for all cells that are \code{NA}, to the nearest cell that is not \code{NA}.
 
The distance unit is in meters if the RasterLayer is not projected (\code{+proj=longlat}) and in map units (typically also meters) when it is projected.
}

\usage{
\S4method{distance}{RasterLayer}(x, filename='', doEdge=TRUE, ...)
}

\arguments{
\item{x}{RasterLayer object}
\item{filename}{Character. Filename for the output RasterLayer (optional)}
\item{doEdge}{Logical. If \code{TRUE}, the \code{\link{edge}} function is called first. This may be efficient in cases where you compute the distance to large blobs. Calling edge determines the edge cells that matter for distance computation}
\item{...}{Additional arguments as for \code{\link{writeRaster}}}
}


\author{Robert J. Hijmans }

\value{RasterLayer}

\seealso{
\code{\link[raster]{distanceFromPoints}},  \code{\link[raster]{gridDistance}},  \code{\link[raster]{pointDistance}}  

See the \code{gdistance} package for more advanced distances, and the \code{geosphere} package for great-circle distances (and more) between points in longitude/latitude coordinates.
}


\examples{
r <- raster(ncol=36,nrow=18)
r[] <- NA
r[500] <- 1
dist <- distance(r) 
#plot(dist / 1000)
}

\keyword{spatial}
