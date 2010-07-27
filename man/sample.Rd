\name{Sample}

\alias{sampleRandom}
\alias{sampleRandom,Raster-method}
\alias{sampleRandom,RasterStack-method}

\alias{sampleRegular}

\title{Random or regular sample}

\description{
Take a random or systematic sample the values from a Raster object. 
}

\usage{
sampleRandom(x, size,...)
sampleRegular(x, size, extent=NULL, cells=FALSE, asRaster=FALSE, corners=FALSE)
}

\arguments{
  \item{x}{a Raster object}
  \item{size}{positive integer giving the number of items to choose.}  
  \item{...}{Additional argument: \code{na.rm}. If \code{TRUE} NA values are removed from random sample}  
  \item{extent}{Extent. To limit regular sampling to the area within that box}  
  \item{cells}{Logical. Also return sampled cell numbers (if asRaster==FALSE) }  
  \item{asRaster}{Logical. If \code{TRUE}, a rasterLayer is returned, rather then the sampled values}  
  \item{corners}{Logical. If \code{TRUE}, the extreme corner cells are always included. This makes for rather irregular sampling when n is small; but it can be useful when asRaster=TRUE, to maintain the same extent as the input RasterLayer}
}

\details{
With na.rm=TRUE, the returned sample may be smaller than requested
}

\value{
A vector, matrix (if cells=TRUE; or for a multi-layered object), or RasterLayer (if asRaster=TRUE)
}

\author{Robert J. Hijmans}

\examples{
 r <- raster(system.file("external/test.grd", package="raster"))
 v <- sampleRegular(r, size=100)
}

\keyword{spatial}
