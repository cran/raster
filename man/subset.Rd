\name{subset}

\alias{subset}
\alias{subset,RasterStack-method}
\alias{subset,RasterBrick-method}

\title{Subset layers in a Raster* object}

\description{
  Extract layers from a RasterStack or RasterBrick object.  
}

\usage{
subset(x, ...) 
}

\arguments{
  \item{x}{ a RasterStack or RasterBrick object }
  \item{...}{Additional arguments. 
	\code{subset}, which should indicate the layers (represented as integer or by their name). 
	\code{drop=TRUE}. If \code{TRUE}, a selection of a single layer will be returned as a RasterLayer  }
}

\value{
  a Raster* object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{dropLayer}}}

\examples{
  r <- raster(system.file("external/test.grd", package="raster"))
  s <- stack(r,r,r)
  sel <- subset(s, 2:3)
  sel <- subset(s, 2)
  sel <- subset(s, 2, drop=FALSE)
  
#  sel <- subset(s, 2:3) is equivalent to
  sel <- dropLayer(s, 1)
}

\keyword{ spatial }
