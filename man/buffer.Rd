\name{buffer}

\alias{buffer}
\alias{buffer,RasterLayer-method}

\title{buffer}

\description{
Calculate a buffer around all cells that are not \code{NA}.
 
Note that the distance unit of the buffer \code{width} parameter is meters if the RasterLayer is not projected (\code{+proj=longlat}), and in map units (typically also meters) when it is projected.
}

\usage{
\S4method{buffer}{RasterLayer}(x, width=0, filename='', doEdge=FALSE, ...)
}

\arguments{
\item{x}{RasterLayer object}
\item{width}{Numeric. Number > 0. Unit is meter if \code{x} has a longitude/latitude CRS, or mapunits in other cases}
\item{filename}{Character. Filename for the output RasterLayer (optional)}
\item{doEdge}{Logical. If \code{TRUE}, the \code{\link{edge}} function is called first. This may be efficient in cases where you compute a buffer around very large areas. Calling edge determines the edge cells that matter for distance computation}
\item{...}{Additional arguments as for \code{\link{writeRaster}}}
}




\value{RasterLayer}

\seealso{
\code{\link[raster]{distance}},  \code{\link[raster]{gridDistance}},  \code{\link[raster]{pointDistance}}  
}


\examples{
r <- raster(ncol=36,nrow=18)
r[] <- NA
r[500] <- 1
b <- buffer(r, width=5000000) 
#plot(b)
}

\keyword{spatial}
