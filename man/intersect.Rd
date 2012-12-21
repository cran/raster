\name{intersect}

\docType{methods}

\alias{intersect}
\alias{intersect,Extent-method}
\alias{intersect,Raster-method}

\title{ 
Intersect Extent
}

\description{
Intersect two Extent objects. Returns the intersection, i.e. the area of overlap of two Extent objects. The second argument can also be any argument from which an Extent object can be extracted. 

If the first object is a Raster* object, this function is equivalent to \code{\link{crop}}. 
}

\usage{
\S4method{intersect}{Extent}(x, y)

\S4method{intersect}{Raster}(x, y)
}

\arguments{
  \item{x}{Extent or Raster* object}
  \item{y}{Extent object, or any object from which an Extent can be extracted}
} 

\value{
Extent or Raster* object
}



\seealso{
\code{\link{union}, \link[raster]{extent}, \link{crop}}
}

\examples{
e1 <- extent(-10, 10, -20, 20)
e2 <- extent(0, 20, -40, 5)
intersect(e1, e2)
}


\keyword{methods}
\keyword{spatial}

