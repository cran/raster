\name{sampleRegular}

\alias{sampleRegular}
\alias{sampleRegular,Raster-method}

\title{Regular sample}

\description{
Take a systematic sample from a Raster* object. 
}

\usage{
\S4method{sampleRegular}{Raster}(x, size, ext=NULL, cells=FALSE, xy=FALSE, asRaster=FALSE, sp=FALSE, useGDAL=FALSE, ...)

}

\arguments{
  \item{x}{Raster object}
  \item{size}{positive integer giving the number of items to choose.}  
  \item{ext}{Extent. To limit regular sampling to the area within that box}  
  \item{cells}{Logical. Also return sampled cell numbers (if asRaster=FALSE) }  
  \item{xy}{logical. If \code{TRUE}, coordinates of sampled cells are also returned}
  \item{asRaster}{Logical. If \code{TRUE}, a RasterLayer or RasterBrick is returned, rather then the sampled values}  
  \item{sp}{logical. If \code{TRUE}, a SpatialPointsDataFrame is returned}  
  \item{useGDAL}{Logical. If \code{TRUE}, GDAL is used to sample in some cases. This is quicker, but can result in values for a different set of cells. Only for rasters that are accessed via rgdal, are not rotated, and when \code{cells=FALSE}}   
  \item{...}{Additional arguments. None implemented}
}

\value{
A vector (single layer object), matrix (multi-layered object; or if \code{cells=TRUE}, or \code{xy=TRUE}), Raster* object (if \code{asRaster=TRUE}), or SpatialPointsDataFrame (if \code{sp=TRUE})
}

\seealso{\code{\link{sampleRandom}}, \link{sampleStratified}}

\author{Robert J. Hijmans}

\examples{
 r <- raster(system.file("external/test.grd", package="raster"))
 v <- sampleRegular(r, size=100)
 x <- sampleRegular(r, size=100, asRaster=TRUE)
}

\keyword{spatial}
