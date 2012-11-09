\name{drawExtent}
\alias{drawExtent}

\title{ Create an Extent object by drawing on a map}

\description{
Click on two points of a plot (map) to obtain an object of class \code{\link{Extent}} ('bounding box')  
}

\usage{
drawExtent(show=TRUE, col="red") 
}

\arguments{
  \item{show}{ Logical. If \code{TRUE}, the extent will be drawn on the map}  
  \item{col}{ Sets the color of the lines of the extent }
 }
 
\value{
 an object of class Extent
} 



\examples{
\dontrun{
r1 <- raster(nrow=10, ncol=10)
r1[] <- runif(ncell(r1))
plot(r1)
e <- drawExtent()
# now click on the map twice
mean(values(crop(r1, drawExtent())))
# now click on the map twice
}
}

\keyword{ spatial }
