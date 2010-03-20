\name{gridDistance}

\alias{gridDistance}


\title{Grid distance}

\description{
 The function calculates the distance to cells of a RasterLayer when the path has to follow the center of neighboring raster cells (currently only implemented as a 'queen' case).
 
 Origin cells should have value TRUE (or 1), destination cells should be \code{NA} and cells that can not be traversed should be set to FALSE (or 0).
 
 The distance is in meters if the RasterLayer is not projected (\code{+proj=longlat}) and in map units (typically meters) when it is projected.
 
 Distances are calculated by summing local distances between cells, which are connected with their neighbours in 8 directions.

 This function currently only works for RasterLayer objects of which all values can be stored in memory 
}

\usage{
gridDistance(x, filename = "", ...) 
}

\arguments{
\item{x}{a RasterLayer object}
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
r <- raster(ncol=36,nrow=18)
r[] <- NA
r[500] <- 1
distRaster <- gridDistance(r) 
plot(distRaster)
}


\keyword{spatial}
