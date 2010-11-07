\name{alignExtent}

\alias{alignExtent}
  
\title{Align an extent (object of class Extent)}

\description{
AlignExtent aligns an extent (Extent object) with the cells of a Raster* object
}

\usage{
alignExtent(extent, object)
}

\arguments{
  \item{extent}{An object of class Extent (which you can create with \code{newExtent()} )}  
  \item{object}{A Raster* object}
}
 
\value{
a object of class Extent
}
 
\details{
Aligning extents assures that the two extents have the same origin and resolution. This should only be used to adjust objects because of imprecision in the data. alignExtent should not be used to force data to match that really does not match (use resample, (dis)aggregate for this).
}
  
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{drawExtent}}, \code{\link[raster]{Extent-class}} }

\examples{
r <- raster()
bb <- extent(-10.1, 10.1, -20.1, 20.1)
bba <- alignExtent(bb, r)
bb
extent(r)
bba

}

\keyword{spatial}
