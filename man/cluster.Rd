\name{Cluster}

\alias{beginCluster}
\alias{endCluster}

\title{Use a multi-core (snow) cluster}

\description{
Create or delete a 'snow' cluster object. This object provides _experimental_ support for multi-core computing with a few 'raster' functions that support it. 

beginCluster creates the cluster object. It determines the number of nodes (cores) that are available and uses all of them (unless the argument \code{n} is used). 

endCluster closes the cluster and removes the object.

The use of the cluster by functions is automatic, but has only been implemented in a few functions.  
}

\usage{
beginCluster(n, type)
endCluster()
}

\arguments{
\item{n}{Integer. The number of nodes to be used (optional)}
\item{type}{Character. The cluster type to be used (optional)}
}

\details{
Functions that are 'cluster-enabled' are: \code{\link{projectRaster}, \link{resample}, \link{distance}}.

For progamming functions see \code{\link{getCluster}, \link{returnCluster}}
}

\value{
None. The side effect is to create or delete a cluster object.
}

\examples{

\dontrun{
beginCluster()

r <- raster(ncol=36, nrow=18)
r[] <- NA
r[500] <- 1
dist <- distance(r) 

endCluster()
}
}

\author{Matteo Mattiuzzi and Robert J. Hijmans}

\keyword{ spatial }

