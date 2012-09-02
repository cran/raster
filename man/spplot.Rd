\name{spplot}

\docType{methods}

\alias{spplot}
\alias{spplot,Raster-method}

\title{Use spplot to plot a Raster* object}

\description{
A wrapper function around \link[sp]{spplot} (sp package). With spplot it is easy to map several layers with a single legend for all maps. ssplot is itself a wrapper around the \link[lattice]{levelplot} function in the lattice package, and see the help for these functions for additional options.

One of the advantages of these wrapper functions is the additional \code{maxpixels} argument to sample large Raster objects for faster drawing.
}

\section{Methods}{
\describe{
\code{spplot(obj, ..., maxpixels=50000, as.table=TRUE)}

\tabular{rll}{
\tab \code{obj} \tab  A Raster* object  \cr
\tab \code{...} \tab  Any argument that can be passed to \code{\link[sp]{spplot}} and \link[lattice]{levelplot} \cr
\tab \code{maxpixels} \tab Integer. Number of pixels to sample from each layer of large Raster objects \cr
}
}}


\seealso{
\code{ \link[raster]{plot}, \link[raster]{plotRGB} }

The rasterVis package has more advanced plotting methods for Raster objects
}

\author{ Robert J. Hijmans}

\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
s <- stack(r, r*2)
names(s) <- c('meuse', 'meuse x 2')

spplot(s)
}

\keyword{methods}
\keyword{spatial}

