\name{pointDistance}

\alias{pointDistance}

\title{Distance between points}

\description{
Calculate the geographic distance between two points on a sphere (\code{type='GreatCircle'}) or on a plane (\code{type='Euclidean'}).
}

\usage{
pointDistance(point1, point2, type='Euclidean', ...)
}

\arguments{
\item{point1}{x and y coordinate of first (set of) point(s), either as c(x, y) or as matrix(ncol=2) in degrees for great-circle distance, in meters (or similar) for Euclidean }
\item{point2}{x and y coordinate of second (set of) second point(s). Like point1. If point1 and point2 are both matrices they should have the same number of rows }
\item{type}{either 'Euclidean' or 'GreatCircle' }
\item{...}{Additional arguments. Can be used to set the radius, \code{r}, of the world (modeled as a sphere), in meters, for use in Great Circle Distance. Default is \code{r=6378137} }

}
\details{
  A sphere is an approximation for the earth (a spheroid) and type='Greatcircle' can thus be used with geographic 
  coordinates (latitude & longitude). 
  You can use type='Euclidean' for coordinates of a map projection such as UTM (but note that in some projections distance is distorted). 
}

\value{
  A single value or a vector of values giving the distance in meters (great-circle distance) or map-units (for instance, meters in the case of UTM)
}

\author{Robert J. Hijmans and Jacob van Etten }

\examples{
   a <- cbind(c(1,5,55,31),c(3,7,20,22))
   b <- cbind(c(4,2,8,65),c(50,-90,20,32))   

   pointDistance(c(0, 0), c(1, 1), type='Euclidean')
   pointDistance(c(0, 0), c(1, 1), type='GreatCircle')
   pointDistance(c(0, 0), a, type='GreatCircle')
   pointDistance(a, b, type='GreatCircle')
   
   #Make a distance matrix (dist object)
   dst <- matrix(nrow=4,ncol=4)
   for (i in 1:4){dst[,i] <- pointDistance(a[i,],a, type='GreatCircle')}
   dst <- as.dist(dst)
   
   #Now the same, but avoiding the loop
   dst <- pointDistance(a[rep(1:4,times=4),],a[rep(1:4,each=4),], type='GreatCircle')
   dst <- as.dist(matrix(dst,ncol=4))
}

\keyword{ spatial }
