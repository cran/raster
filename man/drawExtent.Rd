\name{drawExtent}
\alias{drawExtent}

\title{ Draw extet }
\description{
  Click on two points of a plot (map) to obtain an object of class Extent ('bounding box')  
}
\usage{
drawExtent(show=TRUE, col="red") 
}

\arguments{
  \item{show}{logical. If \code{TRUE}, the bounding box will be drawn on the map}  
  \item{col}{the color of the bounding box lines}    
 }
 
\value{
 an object of class Extent
} 

\author{ Robert J. Hijmans }

\examples{

r1 <- raster(nrow=10, ncol=10)
r1[] <- runif(ncell(r1))
#plot(r1)
#bb <- drawExtent()
#mean(values(crop(r1, drawExtent())))
}


\keyword{ spatial }
