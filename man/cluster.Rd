\name{Cluster}

\alias{beginCluster}
\alias{endCluster}

\title{Use a cluster or multi-core CPU}

\description{
Initialize or delete a 'snow' cluster object to be used by functions that support it. This creates (or removes) the object 'raster_Cluster_raster_Cluster' in the global environment.
}

\usage{
beginCluster(n, type)
endCluster()
}

\arguments{
\item{n}{Integer. The number of nodes to be used (optional)}
\item{type}{Character. The cluster type to be used (optional)}
}

\value{
None. The side effect is to create the cluster object.
}

\author{Matteo Mattiuzzi and Robert J. Hijmans}

\keyword{ spatial }

