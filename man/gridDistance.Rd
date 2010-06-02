\name{gridDistance}

\alias{gridDistance}


\title{Grid distance}

\description{
 The function calculates the distance to cells of a RasterLayer when the path has to follow the center of neighboring raster cells (currently only implemented as a 'queen' case).
 
 The distance is in meters if the RasterLayer is not projected (\code{+proj=longlat}) and in map units (typically meters) when it is projected.
 
 Distances are calculated by summing local distances between cells, which are connected with their neighbours in 8 directions.

}

\usage{
gridDistance(object, originValue, omitValue, filename, ...) 
}

\arguments{
\item{object}{a RasterLayer object}
\item{originValue}{value of the cells from which the distance is calculated}
\item{omitValue}{value of the cells which cannot be traversed}
\item{filename}{character. output filename}
\item{...}{additional arguments. See Details.}  
}


\seealso{ For more options (directions, cost-distance) see the gdistance package on R-forge} 

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\value{A RasterLayer object}


\author{ Jacob van Etten and Robert J. Hijmans }


\examples{

#world lonlat raster
r <- raster(ncol=10,nrow=10)
r[] <- 1
r[48] <- 2
r[66:68] <- 3
distRaster <- gridDistance(r,originValue=2,omitValue=3) 
plot(distRaster)

#UTM small area
projection(r) <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
distRaster <- gridDistance(r,originValue=2,omitValue=3) 
plot(distRaster)

}


\keyword{spatial}
