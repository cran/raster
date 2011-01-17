\name{nudgeExtent}

\alias{roundExtent}
\alias{nudgeExtent}
  
\title{Round extent coordinates}

\description{
\code{roundExtent} rounds the coordinates of the extent of a Raster* to a number of digits specified. 
This can be useful when dealing with a small inprecision in the data (e.g. 179.9999 instead of 180). 
nudgeCoords takes the floor (lower integer) of the mimumum x and y of the exent and the ceiling (upper integer) 
of the maximum x and y of the extent. Thus returning a RasterLayer with an extent of rounded coordinates and that 
always includes the original extent. This can be useful when creating raster objects based on the extent of other objects.
}

\usage{
roundExtent(object, digits=0)
nudgeExtent(object)
}

\arguments{
  \item{object}{ a Raster* object }
  \item{digits}{ integer indicating the precision to be used} 
 }
  
\author{Robert J. Hijmans }
\examples{
r <- raster(xmn=0.999999,  xmx=10.000011, ymn=-60, ymx=60)
r <- roundExtent(r, 2)
r <- nudgeExtent(r)	
}
\keyword{ spatial }

