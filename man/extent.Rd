\name{extent}

\alias{extent}
\alias{extent,Extent-method}
\alias{extent,BasicRaster-method}
\alias{extent,Spatial-method}
\alias{extent,matrix-method}
\alias{extent,numeric-method}
\alias{bbox,Raster-method}

  
\title{Extent}

\description{
This function returns an Extent object of a Raster* or Spatial* object (or an Extent object), or creates an 
Extent object from a matrix (2x2; rows=minx, maxx; cols=miny, maxy) or vector (length=4; order= xmin, xmax, ymin, ymax)

\code{bbox(x)} returns a \code{sp} package like 'bbox' object (a matrix)
}

\usage{
extent(x, ...)
}

\arguments{
  \item{x}{A Raster* or Extent object, a matrix, or four numbers } 
  \item{...}{Additional arguments. Only used when x is numeric (to pass three additional numerics) } 
}
 
\value{
An Extent object
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{drawExtent}} }

\examples{
r <- raster()
extent(r)
extent(c(0, 20, 0, 20))
#is equivalent to
extent(0, 20, 0, 20)
extent(matrix(c(0, 0, 20, 20), nrow=2))
}

\keyword{spatial}
