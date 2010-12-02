\name{alignExtent}

\alias{alignExtent}
  
\title{Align an extent (object of class Extent)}

\description{
Align an Extent object with the cells of a Raster* object
}

\usage{
alignExtent(extent, object)
}

\arguments{
  \item{extent}{An object of class Extent}  
  \item{object}{A Raster* object}
}
 
\value{
a object of class Extent
}
 
\details{
Aligning an Extent object to another object assures that it gets the same origin and resolution. This should only be used to adjust objects because of imprecision in the data. alignExtent should not be used to force data to match that really does not match (use e.g. \code{\link{resample}} or (dis)aggregate for this).
}
  
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{drawExtent}}, \code{\link[raster]{Extent-class}} }

\examples{
r <- raster()
e <- extent(-10.1, 10.1, -20.1, 20.1)
ea <- alignExtent(e, r)
e
extent(r)
ea

}

\keyword{spatial}
