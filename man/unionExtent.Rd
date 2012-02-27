\name{unionExtent}

\alias{unionExtent}
  
\title{Extent union}

\description{
Obsolete. See \code{\link{union}}
}

\usage{
unionExtent(x, ...)
}

\arguments{
  \item{x}{A Extent or Raster* object } 
  \item{...}{ additional Extent or Raster* objects } 
}
 
\value{
Extent object
}
  
\author{Robert J. Hijmans}

\seealso{  \code{\link[raster]{union}}, \code{\link[raster]{intersect}},  \code{\link[raster]{extent}}} 

\examples{
e1 <- extent(-10, 10, -20, 20)
e2 <- extent(0, 20, -40, 5)
unionExtent(e1, e2)
}

\keyword{spatial}
