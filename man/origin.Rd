\name{origin}

\alias{origin}
\alias{origin,BasicRaster-method}

  
\title{Origin}

\description{
Origin returns the coordinates of the point of origin of a Raster* object. This is the point closest to (0, 0) 
that you could get if you moved towards that point in steps of the x and y resolution.
}

\usage{
origin(x)
}

\arguments{
  \item{x}{Raster* object}
}


\value{
A vector of two numbers (x and y coordinates).
}

\seealso{ \code{\link[raster]{ncell}}, \code{\link[raster]{coordinates}} }




\examples{

r <- raster(xmn=-0.5, xmx = 9.5, ncols=10)
origin(r)

}

\keyword{spatial}
