\name{extent}

\alias{extent}
\alias{extent,Extent-method}
\alias{extent,BasicRaster-method}
\alias{extent,Spatial-method}
\alias{extent,matrix-method}
\alias{extent,numeric-method}
\alias{bbox,Raster-method}
\alias{bbox,Extent-method}

  
\title{Extent}

\description{
This function returns an Extent object of a Raster* or Spatial* object (or an Extent object), or creates an 
Extent object from a matrix (2x2; rows=xmin, xmax; cols=ymin, ymax) or vector (length=4; order= xmin, xmax, ymin, ymax)

\code{bbox} returns a \code{sp} package like 'bbox' object (a matrix)
}

\usage{
extent(x, ...)
}

\arguments{
\item{x}{A Raster* or Extent object, a matrix, or a vector of four numbers } 
\item{...}{Additional arguments. When x is a single number you can pass three additional numbers (xmin, xmax, ymin, ymax)

When \code{x} is a Raster* object, you can pass four additional arguments to crop the extent: \code{r1, r2, c1, c2}, representing the first and last row and column number }
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

#crop the extent by row and column numbers
extent(r, 1, 20, 10, 30)
}

\keyword{spatial}
