\name{spplot}

\docType{methods}

\alias{spplot}
\alias{spplot,Raster-method}
\alias{gplot}
\alias{gplot,Raster-method}

\title{Use spplot or ggplot to plot a Raster* object}

\description{
These are simple wrapper functions to around \link[sp]{spplot} (sp package) and \link[ggplot2]{ggplot} (ggplot2 package), allowing for attractive mapping of Raster objects. For example, with spplot it is easy to map several layers with a single legend for all maps. 

Note that the function in the raster package is called gplot with a single 'g'.

One of the advantages of these wrapper functions is the additioal \code{maxpixels} argument to sample large Raster objects for faster drawing.
}

\usage{
gplot(x, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{...}{Additional arguments. See under Details}  
}

\section{Methods}{
\describe{

\code{spplot(obj, maxpixels=50000, ...)}
\code{gplot(x, maxpixels=50000, ...)}

\tabular{rll}{
\tab \code{obj}  \tab  A Raster* object  \cr
\tab \code{maxpixels} \tab Integer. Number of pixels to sample from each layer of large Raster objects \cr
\tab \code{...}  \tab  Any argument that can be passed to \code{\link[sp]{spplot}} or to \code{\link[ggplot2]{ggplot}}  \cr
}
}}


\details{
Most of the code for this function was taken from image.plot (fields package). 
Raster objects with a color-table (e.g. a graphics file) will be plotted according to the color table.
}

\seealso{
  \code{ \link[raster]{plot}, \link[raster]{plot3D}, \link[raster]{plotRGB}, , \link[raster]{persp}, \link[raster]{contour}, \link[raster]{pairs}, \link[raster]{hist}}  
}

\author{ Robert J. Hijmans; gplot based on an example by Paul Hiemstra }

\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
s <- stack(r, r*2)
layerNames(s) <- c('meuse', 'meuse x 2')

spplot(s)

if (require(ggplot2)) {
	theme_set(theme_bw())
	gplot(s) + geom_tile(aes(fill = value)) + facet_wrap(~ variable) +
            scale_fill_gradient(low = 'white', high = 'blue') + coord_equal()
}

}

\keyword{methods}
\keyword{spatial}

