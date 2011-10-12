\name{zoom}

\docType{methods}

\alias{zoom}
\alias{zoom,Raster-method}

\title{Zoom in on a plot}

\description{
Zoom in on a plot (map) by providing a new extent, by default this is done by clicking twice on the map
}

\usage{
zoom(x, ...) 
}

\arguments{
\item{x}{Raster* object }
\item{...}{Additional arguments. See Details}
}

\details{
A full call (with the default values) to zoom is
\code{zoom(x, ext=drawExtent(), maxpixels=100000, layer=1, new=TRUE, ...)}

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{ext} \tab Extent object \cr
\tab \code{maxpixels} \tab Maximum number of pixels used for the map \cr
\tab \code{layer} \tab Positive integer to select the layer to be used if x is a mutilayer Raster object \cr
\tab \code{new} \tab Logical. If \code{TRUE}, the zoomed in map will appear on a new device (window)  \cr
}
}


\value{
An extent object (invisibly)
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{drawExtent}}, \code{\link[raster]{plot}}}

\keyword{ spatial }

