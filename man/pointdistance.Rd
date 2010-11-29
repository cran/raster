\name{pointDistance}

\alias{pointDistance}

\title{Distance between points}

\description{
Calculate the geographic distance between two points on a sphere (\code{latlong=TRUE}) or on a plane (\code{latlong=FALSE}).
}

\usage{
pointDistance(point1, point2, longlat=FALSE, ...)
}

\arguments{
\item{point1}{x and y coordinate of first (set of) point(s), either as c(x, y) or as matrix(ncol=2) in degrees for great-circle distance, in meters (or similar) for Euclidean }
\item{point2}{x and y coordinate of second (set of) second point(s). Like point1. If point1 and point2 are both matrices they should have the same number of rows }
\item{longlat}{Logical. If \code{TRUE}, coordinates should be in degrees; else they should represent 'euclidean' space (e.g. units of meters) }
\item{...}{Additional arguments. Can be used to set the radius, \code{r}, of the world (modeled as a sphere), when \code{longlat=TRUE} Default is \code{r=6378137} }

}

\value{
A single value or a vector of values giving the distance in meters (longlat=TRUE) or map-units (for instance, meters in the case of UTM)
}

\seealso{\code{\link{distanceFromPoints},  \link{distance},  \link{gridDistance}} }


\author{Robert J. Hijmans and Jacob van Etten }

\examples{
a <- cbind(c(1,5,55,31),c(3,7,20,22))
b <- cbind(c(4,2,8,65),c(50,-90,20,32))   

pointDistance(c(0, 0), c(1, 1), longlat=FALSE)
pointDistance(c(0, 0), c(1, 1), longlat=TRUE)
pointDistance(c(0, 0), a, longlat=TRUE)
pointDistance(a, b, longlat=TRUE)
   
#Make a distance matrix (dist object)
dst <- matrix(nrow=4,ncol=4)
for (i in 1:4){dst[,i] <- pointDistance(a[i,],a, longlat=TRUE)}
dst <- as.dist(dst)
   
#The same as above, but avoiding the loop
dst <- pointDistance(a[rep(1:4,times=4),],a[rep(1:4,each=4),], longlat=TRUE)
dst <- as.dist(matrix(dst,ncol=4))
}

\keyword{ spatial }
