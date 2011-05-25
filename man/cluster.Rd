\name{Cluster}

\alias{beginCluster}
\alias{endCluster}

\title{Use a multi-core (snow) cluster}

\description{
Create or delete a 'snow' cluster object. This object provides _experimental_ support for multi-core computing with a few 'raster' functions that support it. 

beginCluster creates the cluster object. It determines the number of nodes (cores) that are available and uses all of them (unless the argument \code{n} is used). 

NOTE: beginCluster may fail when the package 'nws' is installed. You can fix that by removing the 'nws' package, or by setting the cluster type manually, e.g. beginCluster(type="SOCK")

endCluster closes the cluster and removes the object.

The use of the cluster by functions is automatic, but has only been implemented in a few functions.  
}

\usage{
beginCluster(n, type, nice)
endCluster()
}

\arguments{
\item{n}{Integer. The number of nodes to be used (optional)}
\item{type}{Character. The cluster type to be used (optional). E.g. "SOCK", see the snow package for details }
\item{nice}{Integer. To set the prioirty for the workers, between -20 and 20 (UNIX like platforms only)}
}

\details{
Functions that are 'cluster-enabled' are: \code{\link{projectRaster}, \link{resample}, \link{distance},  \link{extract} (when using polygons)}.

If you want to write your own cluster-enabled functions see \code{\link{getCluster}, \link{returnCluster}, and the vignette about writing functions}
}

\value{
None. The side effect is to create or delete a cluster object.
}

\examples{

\dontrun{
beginCluster()
# beginCluster(type="SOCK")

r <- raster(ncol=36, nrow=18)
r[] <- NA
r[500] <- 1
dist <- distance(r) 

endCluster()
}
}

\author{Matteo Mattiuzzi and Robert J. Hijmans}

\keyword{ spatial }

