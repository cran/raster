\name{setMinMax}

\alias{setMinMax,RasterLayer-method}
\alias{setMinMax,RasterStack-method}
\alias{setMinMax,RasterBrick-method}

\alias{setMinMax}

\title{Compute min and max values}

\description{
The minimum and maximum value of a RasterLayer are computed (from a file on disk if necessary) and stored in the returned Raster* object. 
}

\usage{
setMinMax(x)
}

\arguments{
  \item{x}{ A \code{Raster*} object }
}

\value{
a Raster* object
}

\seealso{ \code{\link[raster]{values}} }

\author{ Robert J. Hijmans }

\examples{ 
r <- raster(system.file("pictures/Rlogo.jpg", package="rgdal"))
r
r <- setMinMax(r)
r
	
}

\keyword{ spatial }
\keyword{ methods }

