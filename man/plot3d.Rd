\name{plot3D}

\docType{methods}

\alias{plot3D}
\alias{plot3D,RasterLayer-method}

\title{Interative 3D plot of a RasterLayer}

\description{
Make an interactive 3D plot (map) of a RasterLayer.
}

\section{Methods}{
\describe{

\code{plot3D(x, background=0, maxpixels=100000, zfac=6, col=terrain.colors, rev=TRUE, ...)}

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{background} \tab value used to replace NA values \cr
\tab \code{maxpixels}  \tab  Maximum number of pixels to use  \cr
\tab \code{zfac}  \tab  Numeric, to set the elevation scale relative to x and y \cr
\tab \code{col}  \tab  A color palette, i.e. a vector of n contiguous colors such as \code{rainbow}, \code{heat.colors}, and \code{topo.colors}, on one or your own making \cr
\tab \code{rev}  \tab  Logical. If \code{TRUE}, the color palette values are reversed in order \cr
\tab \code{...}  \tab  Any argument that can be passed to \code{\link[rgl]{plot3d}}  \cr
}
}}

\author{ Robert J. Hijmans }

\keyword{methods}
\keyword{spatial}

