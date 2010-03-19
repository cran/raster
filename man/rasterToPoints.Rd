\name{rasterToPoints}

\alias{rasterToPoints}
\alias{rasterToPolygons}

\title{ Raster to points conversion}

\description{
Raster to point or polygon conversion. Cells with NA are not converted. A function can be used to select a subset of the raster cells (by their values).
}

\usage{
rasterToPoints(raster, fun=NULL, asSpatialPoints=FALSE)
rasterToPolygons(raster, fun=NULL)
}

\arguments{
 \item{raster}{ a RasterLayer object }
 \item{fun}{ function to select a subset of raster values }
 \item{asSpatialPoints}{if \code{TRUE}, the function returns a SpatialPointsDataFrame object }
}

\details{
\code{fun} should be a simple function returning a logical value
e.g.: \code{fun=function(x){x==1}} or  \code{fun=function(x){x>3}}
}

\value{
A matrix with three columns: x, y, and v (value)
}

\author{Robert J. Hijmans}

\examples{ 
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r)) * 10
r[r>8] <- NA
p <- rasterToPoints(r)
p <- rasterToPoints(r, fun=function(x){x>6})
pol <- rasterToPolygons(r, fun=function(x){x>6})
#plot(r)
#points(p)
#plot(pol, add=T, col='red')
}

\keyword{ spatial }
