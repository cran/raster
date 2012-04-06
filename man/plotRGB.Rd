\name{plotRGB}

\docType{methods}

\alias{plotRGB}
\alias{plotRGB,RasterStackBrick-method}

\title{Red-Green-Blue plot of a multi-layered Raster object}

\description{
Make a Red-Green-Blue plot based on three layers (in a RasterBrick or RasterStack). Three layers (bands) are combined such that one is the red channel, one is the green channel, and one is the blue channel. This function could be used to make 'True (or false) color images' from Landsat and other multi-band satellite images. 
}



\usage{
\S4method{plotRGB}{RasterStackBrick}(x, r=1, g=2, b=3, scale, maxpixels=500000, stretch=NULL, ext=NULL, interpolate=FALSE, bgcol='white', alpha, bgalpha, ...)
}

\arguments{
  \item{x}{RasterBrick or RasterStack}
  \item{r}{Integer. Index of the Red channel, between 1 and nlayers(x)}
  \item{g}{Integer. Index of the Green channel, between 1 and nlayers(x)}
  \item{b}{Integer. Index of the Blue channel, between 1 and nlayers(x)}
  \item{scale}{Integer. Maximum (possible) value in the three channels. Defaults to 255 or to the maximum value of \code{x} if that is known and larger than 255}
  \item{maxpixels}{Maximum number of pixels to use}
  \item{stretch}{Option to stretch the values to increase the contrast of the image: "lin" or "hist"}
  \item{ext}{An \code{\link{Extent}} object to zoom in to a region of interest (see \code{\link{drawExtent}}}
  \item{interpolate}{interpolate the image when drawing}
  \item{bgcol}{Background (NA) color. Default is 'white'}
  \item{alpha}{transparancy. Integer between 0 (transparant) and 255 (opaque)}
  \item{bgalpha}{Background transparancy. Integer between 0 (transparant) and 255 (opaque)}
  \item{...}{graphical parameters as in \code{\link{rasterImage}}}
}

\author{ Robert J. Hijmans; stretch option based on functions by Josh Gray }

\seealso{ \code{\link[raster]{plot}} }

\examples{
b <- brick(system.file("external/rlogo.grd", package="raster"))
plotRGB(b)
plotRGB(b, 3, 2, 1)
plotRGB(b, 3, 2, 1, stretch='hist')
}

\keyword{methods}
\keyword{spatial}

