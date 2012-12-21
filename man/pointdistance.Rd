\name{pointDistance}

\alias{pointDistance}

\title{Distance between points}

\description{
Calculate the geographic distance between two (sets of) points on a sphere (\code{longlat=TRUE}) or on a plane (\code{longlat=FALSE}).
}

\usage{
pointDistance(p1, p2, longlat, ...)
}

\arguments{
\item{p1}{x and y coordinate of first (set of) point(s), either as c(x, y), matrix(ncol=2), or SpatialPoints*. }
\item{p2}{x and y coordinate of second (set of) second point(s) (like for \code{p1}). If this argument is missing, a distance matrix is computed for \code{p1} }
\item{longlat}{Logical. If \code{TRUE}, coordinates should be in degrees; else they should represent planar ('Euclidean') space (e.g. units of meters) }
\item{...}{Additional arguments. Can be used to set the radius, \code{r}, of the world (modeled as a sphere), when \code{longlat=TRUE} Default is \code{r=6378137} }
}

\value{
A single value, or a vector, or matrix of values giving the distance in meters (longlat=TRUE) or map-units (for instance, meters in the case of UTM)
If \code{p2} is missing, a distance matrix is returned
}

\seealso{\code{\link{distanceFromPoints}, \link{distance}, \link{gridDistance}}, \code{\link[sp]{spDistsN1}}. 
The \code{geosphere} package has many additional distance functions and other functions that operate on spherical coordinates}


\author{Robert J. Hijmans and Jacob van Etten }

\examples{
a <- cbind(c(1,5,55,31),c(3,7,20,22))
b <- cbind(c(4,2,8,65),c(50,-90,20,32))   

pointDistance(c(0, 0), c(1, 1), longlat=FALSE)
pointDistance(c(0, 0), c(1, 1), longlat=TRUE)
pointDistance(c(0, 0), a, longlat=TRUE)
pointDistance(a, b, longlat=TRUE)
   
#Make a distance matrix 
dst <- pointDistance(a, longlat=TRUE)
# coerce to dist object
dst <- as.dist(dst)
}

\keyword{ spatial }
