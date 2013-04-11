\name{mask}

\docType{methods}

\alias{mask}
\alias{mask,RasterLayer,RasterLayer-method}
\alias{mask,RasterStackBrick,RasterLayer-method}
\alias{mask,RasterLayer,RasterStackBrick-method}
\alias{mask,RasterStackBrick,RasterStackBrick-method}
\alias{mask,Raster,Spatial-method}

\title{Mask values in a Raster object}

\description{
Create a new Raster* object that has the same values as Raster* \code{x}, except for the cells that are \code{NA}  in a 'mask'. These cells become \code{NA}. The mask can be either another Raster* object of the same extent and resolution, or a Spatial* object (e.g. SpatialPolygons) in which case all cells that are not covered by the Spatial object are set to \code{NA}. You can use \code{inverse=TRUE} to set the cells that are not \code{NA} in the mask, or not coverd by the Spatial* object to \code{NA}. When the mask is a Raster* object, you can also use another value than \code{NA} in the mask object}

\usage{
\S4method{mask}{RasterLayer,RasterLayer}(x, mask, filename="", inverse=FALSE, maskvalue=NA, ...)

\S4method{mask}{RasterStackBrick,RasterLayer}(x, mask, filename="", inverse=FALSE, maskvalue=NA, ...)

\S4method{mask}{RasterLayer,RasterStackBrick}(x, mask, filename="", inverse=FALSE, maskvalue=NA, ...)

\S4method{mask}{RasterStackBrick,RasterStackBrick}(x, mask, filename="", inverse=FALSE, maskvalue=NA, ...)

\S4method{mask}{Raster,Spatial}(x, mask, filename="", inverse=FALSE, ...)
}


\arguments{
\item{x}{Raster* object}
\item{mask}{Raster* object or a Spatial* object}
\item{inverse}{logical. If \code{TRUE}, areas on mask that are _not_ \code{NA} are masked. This option is only relevant if ]code{maskvalue=NA}}
\item{maskvalue}{Numeric. The value in \code{mask} that indicates the cells of \code{x} that should become \code{NA}}
\item{filename}{Character. Optional output filename}
\item{...}{Additional arguments as in \code{\link{writeRaster}}}
}

\value{Raster* object}

\seealso{\code{\link{rasterize}, \link{crop}}}


\examples{
r <- raster(ncol=10, nrow=10)
m <- raster(ncol=10, nrow=10)
r[] <- runif(ncell(r)) * 10
m[] <- runif(ncell(r))
m[m < 0.5] <- NA
mr <- mask(r, m)

m2 <- m > .7
mr2 <- mask(r, m2, maskvalue=TRUE)
}

\keyword{methods}
\keyword{spatial}

