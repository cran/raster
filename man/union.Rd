\name{union}

\docType{methods}

\alias{union}
\alias{union,Extent,Extent-method}

\title{ 
Union Extent
}

\description{
Union of two Extent objects. See \code{\link{crop}} and \code{\link{expand}} to union a Raster object with an Extent object.

}

\usage{
\S4method{union}{Extent,Extent}(x, y)
}

\arguments{
  \item{x}{Extent}
  \item{y}{Extent}
} 

\value{
Extent
}

\author{Robert J. Hijmans}

\seealso{
\code{\link[raster]{intersect}, \link[raster]{extent}, \link[raster]{setExtent}}
} 

\examples{
e1 <- extent(-10, 10, -20, 20)
e2 <- extent(0, 20, -40, 5)
union(e1, e2)
}


\keyword{methods}
\keyword{spatial}

