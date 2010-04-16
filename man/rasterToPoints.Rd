\name{rasterToPoints}

\alias{rasterToPoints}

\title{ Raster to points conversion}

\description{
Raster to point conversion. Cells with NA are not converted. A function can be used to select a subset of the raster cells (by their values).
}

\usage{
rasterToPoints(x, fun=NULL, asSpatialPoints=FALSE)
}

\arguments{
 \item{x}{ a Raster object }
 \item{fun}{ function to select a subset of raster values }
 \item{asSpatialPoints}{if \code{TRUE}, the function returns a SpatialPointsDataFrame object }
}

\details{
\code{fun} should be a simple function returning a logical value
e.g.: \code{fun=function(x){x==1}} or  \code{fun=function(x){x>3}}
}

\value{
A matrix with three columns: x, y, and v (value), or a SpatialPointsDataFrame object
}

\author{Robert J. Hijmans}

\examples{ 
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r)) * 10
r[r>8] <- NA
p <- rasterToPoints(r)
p <- rasterToPoints(r, fun=function(x){x>6})
#plot(r)
#points(p)
}

\keyword{ spatial }
