\name{rasterFromXYZ}

\alias{rasterFromXYZ}

\title{ Create a RasterLayer from x, y, z values }

\description{
Create a RasterLayer from x, y, z values. x and y must be on a regular grid. If the resolution is not supplied, it is assumed to be the minimum distance between x and y coordinates, but a resolution of up to 10 times smaller is evaluated if a regular grid cannot be created. 
If the exact properties of the RasterLayer are known beforehand, it may be preferable to simply create a new RasterLayer with the raster function instead, and assign the values in the standard R way (see example below).
}

\usage{
rasterFromXYZ(xyz, res=c(NA,NA), crs=NA, digits = 6)
}

\arguments{
\item{xyz}{Matrix or data.frame with three colomns: x and y coordinates and value z}
\item{res}{The x and y cell resolutin (optional)}
\item{crs}{A CRS object or a character string describing a projection and datum in PROJ4 format}
\item{digits}{Precision for detecting wether points are on a regular grid (i.e., a low number of digits is a low precision)}
}

\value{
RasterLayer
}

 \seealso{ For random-like point distributions, see \code{\link[raster]{pointsToRaster}} }

\author{Robert J. Hijmans }

\examples{
r <- raster(nrow=10, ncol=10, xmn=0, xmx=10, ymn=0, ymx=10, crs=NA)
r[] <- runif(ncell(r))
r[r<0.5] <- NA
xyz <- rasterToPoints(r)

r2 <- rasterFromXYZ(xyz)

# equivalent to:
r3 <- raster(nrow=10, ncol=10, xmn=0, xmx=10, ymn=0, ymx=10)
cells <- cellFromXY(r3, xyz[,1:2])
r3[cells] <- xyz[,3]
}

\keyword{methods}
\keyword{spatial}
