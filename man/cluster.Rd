\name{cluster}

\alias{beginCluster}
\alias{endCluster}
\alias{clusterR}

\title{Use a multi-core cluster}

\description{
\code{beginCluster} creates, and \code{endCluster} deletes a 'snow' cluster object. This object can be used for multi-core computing with those 'raster' functions that support it. 

\code{beginCluster} determines the number of nodes (cores) that are available and uses all of them (unless the argument \code{n} is used). 

NOTE: beginCluster may fail when the package 'nws' is installed. You can fix that by removing the 'nws' package, or by setting the cluster type manually, e.g. \code{beginCluster(type="SOCK")}

endCluster closes the cluster and removes the object.

The use of the cluster is automatic in these functions" \code{ \link{predict}, \link{projectRaster}, \link{resample}} and in \code{\link{extract}} when using polygons.

\code{clusterR} is a flexible interface for using cluster with other functions. This function only works with functions from the raster package that return Raster* objects that operate on a cell by cell basis and return an object with the same number of cells as the input raster object(s). Among other functions, it does _not_ work with mrege, crop, mosaic, (dis)aggregate, resample, projectRaster, focal, distance, direction.
}

\usage{
beginCluster(n, type, nice, exclude)
endCluster()
clusterR(x, fun, args=NULL, filename='', cl=NULL, m=2, ...)
}

\arguments{
\item{n}{Integer. The number of nodes to be used (optional)}
\item{type}{Character. The cluster type to be used (optional). E.g. "SOCK", see the snow package for details }
\item{nice}{Integer. To set the prioirty for the workers, between -20 and 20 (UNIX like platforms only)}
\item{exclude}{Character. Packages to exclude from loading on the nodes (because they may fail there) but are required/loaded on the master }
\item{x}{Raster* object}
\item{fun}{function}
\item{args}{arguments for the function (excluding x, which should always be the first argument}
\item{filename}{character. Output filename (optional)}
\item{cl}{cluster object (do not use it if beginCluster() has been called}
\item{m}{tuning parameter to determine how many blocks should be used. The number is rounded and multiplied with the number of nodes.}
\item{...}{additional arguments for writing Raster objects to disk}
}

\note{
If you want to write your own cluster-enabled functions see \code{\link{getCluster}, \link{returnCluster}}, and the vignette about writing functions
}

\value{
None. The side effect is to create or delete a cluster object.
}

\examples{

\dontrun{

#beginCluster()
beginCluster(type="SOCK")

r <- raster()
r[] <- 1:ncell(r)

x <- clusterR(r, sqrt, verbose=T)

f1 <- function(x) calc(x, sqrt)
y <- clusterR(r, f1)

s <- stack(r, r*2, r*3)
f2 <- function(x) calc(x, range)
z <- clusterR(s, f2)

pts <- matrix(c(0,0, 45,45), ncol=2, byrow=T)
d <- clusterR(r, distanceFromPoints, args=list(xy=pts))

endCluster()
}
}

\author{Matteo Mattiuzzi and Robert J. Hijmans}

\keyword{ spatial }

