\name{rasterToPolygons}

\alias{rasterToPolygons}

\title{ Raster to polygons conversion}

\description{
Raster to polygon conversion. Cells with NA are not converted. A function can be used to select a subset of the raster cells (by their values).
}

\usage{
rasterToPolygons(raster, fun=NULL, digits=12)
}

\arguments{
 \item{raster}{ a RasterLayer object }
 \item{fun}{ function to select a subset of raster values }
 \item{digits}{ number of digits to round the coordinates to }
}

\details{
\code{fun} should be a simple function returning a logical value
e.g.: \code{fun=function(x){x==1}} or  \code{fun=function(x){x>3 & x<6}}
}

\value{
A SpatialPolygonsDataFrame object
}

\author{Robert J. Hijmans}

\examples{ 
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r)) * 10
r[r>8] <- NA
pol <- rasterToPolygons(r, fun=function(x){x>6})
#plot(r)
#plot(pol, add=T, col='red')
}

\keyword{ spatial }
