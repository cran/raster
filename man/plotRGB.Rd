\name{plotRGB}

\docType{methods}

\alias{plotRGB}
\alias{plotRGB,RasterStackBrick-method}

\title{Red-Green-Blue plot of a multi-layered Raster object}

\description{
Make a Red-Green-Blue plot based on three layers (in a RasterBrick or RasterStack). Three layers (bands) are combined such that one is the red channel, one is the green channel, and one is the blue channel. This function could be used to make 'True (or false) color images' from Landsat and other multi-band satellite images. 
}

\section{Methods}{
\describe{

\code{plotRGB(x, r=1, g=2, b=3, scale=255, maxpixels=500000, extent=NULL, ...)}

\tabular{rll}{
\tab \code{x} \tab a RasterBrick or RasterStack object \cr
\tab \code{r}  \tab  Integer. Index of the Red channel, between 1 and nlayers(x) \cr
\tab \code{g}  \tab  Integer. Index of the Green channel, between 1 and nlayers(x) \cr
\tab \code{b}  \tab  Integer. Index of the Blue channel, between 1 and nlayers(x)\cr
\tab \code{scale}  \tab Integer. Maximum (possible) value in the three channels \cr 
\tab \code{maxpixels}  \tab  Maximum number of pixels to use  \cr
\tab \code{extent}  \tab  An extent object to zoom in a region \cr
\tab \code{...} \tab Any argument that can be passed to \code{\link[graphics]{image}} (graphics package) \cr
}
}}

\author{ Robert J. Hijmans; based on functions in the pixmap package by Friedrich Leisch }

\seealso{ \code{\link[raster]{plot}} }

\examples{
b <- brick(system.file("external/rlogo.grd", package="raster"))
plotRGB(b)
plotRGB(b, 3, 2, 1)
}

\keyword{methods}
\keyword{spatial}

