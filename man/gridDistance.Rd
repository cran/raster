\name{gridDistance}

\alias{gridDistance}


\title{Grid distance}

\description{
 The function calculates the distance to cells of a RasterLayer when the path has to follow the center of neighboring raster cells (currently only implemented as a 'queen' case).
 
 The distance is in meters if the RasterLayer is not projected (\code{+proj=longlat}) and in map units (typically meters) when it is projected.
 
 Distances are calculated by summing local distances between cells, which are connected with their neighbours in 8 directions.
}

\usage{
gridDistance(x, origin, omit=NULL, filename="", ...) 
}

\arguments{
\item{x}{a RasterLayer object}
\item{origin}{value(s) of the cells from which the distance is calculated}
\item{omit}{value(s) of the cells which cannot be traversed (optional)}
\item{filename}{character. output filename (optional)}
\item{...}{additional arguments. See Details.}  
}


\seealso{ See \code{\link[raster]{distance}} for 'as the crow flies' distance. Additional distance measures and options (directions, cost-distance) are available in the '\code{gdistance}' package.} 


\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
 
 If the RasterLayer to be processed is big, it will be processed in chunks automatically. This may lead to errors in the case of complex objects spread over different chunks (meandering rivers, for instance). You can try to solve these issues by varying the chunk size, see function setOptions().
 
}

\value{A RasterLayer object}


\author{ Jacob van Etten and Robert J. Hijmans }


\examples{

#world lonlat raster
r <- raster(ncol=10,nrow=10)
r[] <- 1
r[48] <- 2
r[66:68] <- 3
d <- gridDistance(r,origin=2,omit=3) 
plot(d)

#UTM small area
projection(r) <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
d <- gridDistance(r,origin=2,omit=3) 
plot(d)

}


\keyword{spatial}
